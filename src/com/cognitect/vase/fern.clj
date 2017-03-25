(ns com.cognitect.vase.fern
  (:require [instaparse.core :as insta]
            [com.cognitect.vase.literals :as lit]
            [clojure.string :as str]))

(def ^:private ^:dynamic *input* nil)

(def fern-parser
  (insta/parser
   "Description = ( Schema | Api | Spec )*
    Schema = <'schema'> keyword-name (Attribute | SchemaUse)*

    Attribute = <'attribute'> keyword-name cardinality kind toggle* quotedstring
    cardinality = 'one' | 'many'
    kind = 'long' | 'double' | 'instant' | 'ref' | 'bigint' | 'float' | 'string' | 'keyword' | 'bigdec' | 'bytes' | 'uri' | 'uuid' | 'boolean'
    toggle = 'unique' | 'identity' | 'index' | 'fulltext' | 'component' | 'no-history'

    Api = <'api'> keyword-name (Route | SchemaUse)*
    Route = Verb Path InterceptQueue
    Verb = 'get' | 'put' | 'post' | 'delete' | 'head' | 'options'
    <Path> = quotedstring
    InterceptQueue = InterceptorRef | InterceptorRefs
    InterceptorRef = qualified-name
    InterceptorRefs = <'['> qualified-name+ <']'>

    SchemaUse = <'schema'> <'use'> keyword-name

    StockInterceptor = Respond | Redirect | Interceptor | Conform | Query | Transact

    Respond = <'respond'> keyword-name RespondClause*
    <RespondClause> = status | params | headers | edn-coerce | body

    Redirect = <'redirect'> keyword-name RedirectClause*
    <RedirectClause> = status | params | headers | body | url-literal | url-expr

    Interceptor = <'interceptor'> keyword-name InterceptorClause*
    <InterceptorClause> = EnterClause | LeaveClause | ErrorClause
    EnterClause = <'enter'> s-expr
    LeaveClause = <'leave'> s-expr
    ErrorClause = <'error'> s-expr

    Conform = <'conform'> keyword-name ConformClause*
    <ConformClause> = from | to | with-spec
    with-spec = <'with-spec'> s-expr

    Query = <'query'> keyword-name QueryClause*
    <QueryClause> = to | headers | q | params
    q = <'q'> s-expr

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
    body = <'body'> s-expr
    url-literal = <'url'> quotedstring
    url-expr = <'url'> s-expr

    Spec = <'spec'> keyword-name s-expr

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
"  :auto-whitespace :standard))

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
                         (let [schemas (filter #(= :schema (first %)) parts)
                               routes  (remove #(= :schema (first %)) parts)]
                           {nm
                            (cond-> {}
                              (seq schemas)
                              (assoc :vase.api/schemas (into [] (map second schemas)))

                              (seq routes)
                              (assoc :vase.api/routes (apply merge-with merge routes)))}))
   :Spec               (fn [nm spec] {nm (native spec)})
   :SchemaUse          (keyed :schema identity)
   :Route              (fn [verb path ints] {path {verb ints}})
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
   :params             (keyed :params vector)
   :param-with-default (fn [nm default] (vector nm (native default)))
   :edn-coerce         (keyed :edn-coerce vector)
   :headers            (keyed :headers maplike)
   :body               (keyed :body native)
   :url-literal        (keyed :url identity)
   :url-expr           (keyed :url native)
   :with-spec          (keyed :spec native)
   :q                  (keyed :query native)
   :EnterClause        (keyed :enter native)
   :LeaveClause        (keyed :leave native)
   :ErrorClause        (keyed :error native)})

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
