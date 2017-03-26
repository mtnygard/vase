(ns com.cognitect.vase.fern
  (:require [clojure.string :as str]
            [com.cognitect.vase.literals :as lit]
            [instaparse.core :as insta]
            [io.pedestal.http :as http]))

(def ^:private ^:dynamic *input* nil)

(def whitespace-or-comments
  (insta/parser
   "ws-or-comments = #\"\\s+\" | comments
    comments = comment+
    comment = ';' #\".*?(\\n|\\z)\"    "
   :auto-whitespace :standard))

(def fern-parser
  (insta/parser
   "Description = Http? | ( Schema | Api | Spec )*
    Http = <'http'> HttpClause*
    HttpClause = HttpKeyword NativeExpr
    HttpKeyword = 'allowed-origins' | 'container-options' | 'enable-csrf' | 'enable-session' | 'file-path' | 'host' | 'interceptors' | 'method-param-name' | 'mime-types' | 'not-found-interceptor' | 'port' | 'resource-path' | 'router' | 'secure-headers' | 'type'
    NativeExpr = s-expr

    Schema = <'schema'> keyword-name (Attribute | SchemaUse)*

    Attribute = <'attribute'> keyword-name cardinality kind toggle* quotedstring
    cardinality = 'one' | 'many'
    kind = 'long' | 'double' | 'instant' | 'ref' | 'bigint' | 'float' | 'string' | 'keyword' | 'bigdec' | 'bytes' | 'uri' | 'uuid' | 'boolean'
    toggle = 'unique' | 'identity' | 'index' | 'fulltext' | 'component' | 'no-history'

    Api = <'api'> keyword-name (Route / SchemaUse / SchemaInline)*
    Route = Verb Path InterceptQueue
    Verb = 'get' | 'put' | 'post' | 'delete' | 'head' | 'options'
    <Path> = quotedstring
    InterceptQueue = InterceptorRef | InterceptorRefs
    InterceptorRef = qualified-name
    InterceptorRefs = <'['> qualified-name+ <']'>

    SchemaUse = <'schema'> <'use'> keyword-name
    SchemaInline = Schema

    StockInterceptor = Respond | Redirect | Interceptor | Conform | Query | Transact

    Respond = <'respond'> keyword-name RespondClause*
    <RespondClause> = status | params | headers | edn-coerce | body

    Redirect = <'redirect'> keyword-name RedirectClause*
    <RedirectClause> = status | params | headers | body | url

    Interceptor = <'interceptor'> keyword-name InterceptorClause*
    InterceptorClause = InterceptorKeyword NativeExpr
    InterceptorKeyword = 'enter' | 'leave' | 'error'

    Conform = <'conform'> keyword-name ConformClause*
    <ConformClause> = from | to | with-spec
    with-spec = <'with-spec'> NativeExpr

    Query = <'query'> keyword-name QueryClause*
    <QueryClause> = to | headers | q | params
    q = <'q'> NativeExpr

    Transact = <'transact'> keyword-name TransactClause*
    <TransactClause> = to | headers | params | operation
    operation = <'operation'> keyword-name

    from = <'from'> keyword-name
    to = <'to'> keyword-name

    status = <'status'> integer
    params = <'params'> <'['> (keyword-name | param-with-default)+ <']'>
    param-with-default = <'['> keyword-name clojure-tok <']'>
    headers = <'headers'> <'{'> ( quotedstring quotedstring )+ <'}'>
    edn-coerce = <'edn-coerce'> <'['> keyword-name+ <']'>
    body = <'body'> NativeExpr
    url = <'url'> NativeExpr

    Spec = <'spec'> keyword-name NativeExpr

    s-expr = list-expr | vec-expr | map-expr | reader-macro | clojure-tok
    list-expr = '(' (s-expr / clojure-tok)* ')'
    vec-expr = '[' (s-expr / clojure-tok)* ']'
    map-expr = '{' (s-expr / clojure-tok)* '}'
    reader-macro = '#(' (s-expr / clojure-tok)* ')'
    clojure-tok = #\"[#a-zA-Z?'.:_\\-0-9*+$&%^.,/\\\"<>=]+\"

    quotedstring = #'\"[^\"]*\"'
    keyword-name = qualified-name
    qualified-name =  ( namespace )? name
    namespace = ident ( '.' ident)* <'/'>
    name = ident ( '.' ident )*
    integer = #\"[1-9][0-9]*\"
    <ident> = #'[A-Za-z_][A-Za-z0-9_\\-\\?!$\\%]*'
"  :auto-whitespace whitespace-or-comments))

(defn- build-attribute
  [& attr-vec]
  (lit/parse-schema-vec attr-vec))

(defn- vectorize [& xs] (vec xs))
(defn- symbolify [& parts] (symbol (apply str parts)))

(defn- maplike
  [& pairs]
  (reduce #(apply assoc %1 %2) {} (partition 2 pairs)))

(defn- snip-input
  [node]
  (when-let [m (meta node)]
    (read-string (subs *input*
                       (inc (:instaparse.gll/start-index m))
                       (:instaparse.gll/end-index m)))))

(defn- native
  ([expr]
   (snip-input expr))
  ([expr & exprs]
   (mapv snip-input (list* expr exprs))))

(defn- keyed
  [k f]
  (fn [& args]
    [k (apply f args)]))

(defn- val-mapped
  [m]
  (fn [& args]
    (zipmap (keys m) (map #(apply % args) (vals m)))))

(defn- rename-key
  [m old new]
  (dissoc (assoc m new (m old)) old))

(def transforms
  {:integer            #(Integer. %)
   :name               symbolify
   :namespace          symbolify
   :qualified-name     (fn
                         ([nm-part]         nm-part)
                         ([ns-part nm-part] (symbol (str ns-part) (str nm-part))))
   :keyword-name       keyword
   :quotedstring       (fn [s] (apply str (drop 1 (butlast s))))
   :Attribute          build-attribute
   :cardinality        keyword
   :kind               keyword
   :toggle             keyword
   :Http               (fn [& parts]
                         {:fern/http (reduce merge {} parts)})
   :Schema             (fn [nm & parts]
                         (let [schemas (filter #(= :schema (first %)) parts)
                               attrs   (remove #(= :schema (first %)) parts)]
                           {nm
                            (cond-> {}
                              (seq schemas)
                              (assoc :vase.norm/requires (into [] (map second schemas)))

                              (seq attrs)
                              (assoc :vase.norm/txes attrs))}))
   :Api                (fn [nm & parts]
                         (let [{:keys [route schema schemainline]} (group-by first parts)]
                           {nm
                            (cond-> {}
                              (seq schema)
                              (assoc :vase.api/schemas (into [] (map second schema)))

                              (seq schemainline)
                              (assoc :fern.api/schema (apply merge-with merge (map second schemainline)))

                              (seq route)
                              (assoc :vase.api/routes (apply merge-with merge (map second route))))}))
   :Spec               hash-map
   :SchemaUse          (keyed :schema identity)
   :SchemaInline       (keyed :schemainline identity)
   :Route              (keyed :route (fn [verb path ints] {path {verb ints}}))
   :Verb               keyword
   :InterceptQueue     identity
   :InterceptorRef     vector
   :InterceptorRefs    vectorize
   :StockInterceptor   identity
   :Respond            (fn [nm & clauses]
                         (lit/map->RespondAction (into {:name nm} clauses)))
   :Redirect           (fn [nm & clauses]
                         (lit/map->RedirectAction (into {:name nm} clauses)))
   :Interceptor        (fn [nm & clauses]
                         (lit/map->InterceptAction (into {:name nm} clauses)))
   :Conform            (fn [nm & clauses]
                         (lit/map->ConformAction (into {:name nm} clauses)))
   :Query              (fn [nm & clauses]
                         (lit/map->QueryAction (into {:name nm} clauses)))
   :Transact           (fn [nm & clauses]
                         (lit/map->TransactAction (rename-key (into {:name nm} clauses) :params :properties)))
   :HttpClause         hash-map
   :HttpKeyword        (fn [kw] (keyword "io.pedestal.http" kw))
   :NativeExpr         native
   :params             (keyed :params vector)
   :param-with-default (fn [nm default] (vector nm (native default)))
   :edn-coerce         (keyed :edn-coerce vector)
   :headers            (keyed :headers maplike)
   :body               (keyed :body identity)
   :with-spec          (keyed :spec identity)
   :q                  (keyed :query identity)
   :InterceptorKeyword keyword
   :InterceptorClause  hash-map})

(defn transform
  [parse-tree]
  (if (insta/failure? parse-tree)
    parse-tree
    (insta/transform transforms parse-tree)))

(defn parse-string
  "Parses the contents of a string as 'Fern' language for defining
  Vase APIs. See the namespace docstring for a definition of Fern
  syntax.

  On success, returns the parse tree. Pass the parse tree to
  `transform` to turn it into a full Vase descriptor.

  On parse failure, returns data about the parse failure. Pass that to
  `describe-error` to get a human-readable explanation of the problem."
  ([content]
   (parse-string content :Description))
  ([content start]
   (binding [*input* content]
     (transform
      (insta/parse fern-parser content :start start)))))
