(ns com.cognitect.vase.fern
  (:require [instaparse.core :as insta]
            [com.cognitect.vase.literals :as lit]
            [clojure.string :as str]))

(def ^:private ^:dynamic *input* nil)

(def fern-parser
  (insta/parser
   "Description = Schema*
    Schema = <'schema'> qualified-name Attribute*

    Attribute = <'attribute'> keyword-name cardinality kind toggle* quotedstring
    cardinality = 'one' | 'many'
    kind = 'long' | 'double' | 'instant' | 'ref' | 'bigint' | 'float' | 'string' | 'keyword' | 'bigdec' | 'bytes' | 'uri' | 'uuid' | 'boolean'
    toggle = 'unique' | 'identity' | 'index' | 'fulltext' | 'component' | 'no-history'

    Api = <'api'> qualified-name Route*
    Route = Verb Path InterceptQueue
    Verb = 'get' | 'put' | 'post' | 'delete' | 'head' | 'options'
    <Path> = quotedstring
    InterceptQueue = InterceptorRef | InterceptorRefs
    InterceptorRef = qualified-name
    InterceptorRefs = <'['> qualified-name+ <']'>

    StockInterceptor = Respond | Redirect | Interceptor | Conform | Query

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
    <ConformClause> = from | to | spec
    spec = <'spec'> s-expr

    Query = <'query'> keyword-name QueryClause*
    <QueryClause> = to | headers | q | params
    q = <'q'> s-expr

    from = <'from'> keyword-name
    to = <'to'> keyword-name

    status = <'status'> integer
    params = <'params'> <'['> keyword-name+ <']'>
    headers = <'headers'> <'{'> ( quotedstring quotedstring )+ <'}'>
    edn-coerce = <'edn-coerce'> <'['> keyword-name+ <']'>
    body = <'body'> s-expr
    url-literal = <'url'> quotedstring
    url-expr = <'url'> s-expr

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

(defn- vectorize [& xs] (vec xs))
(defn- symbolify [& parts] (symbol (apply str parts)))

(defn- maplike
  [& pairs]
  [:headers (reduce #(apply assoc %1 %2) {} (partition 2 pairs))])

(defn- snip-input
  [node]
  (when-let [m (meta node)]
    (read-string (subs *input*
                       (inc (:instaparse.gll/start-index m))
                       (:instaparse.gll/end-index m)))))

(defn native
  ([expr]
   (snip-input expr))
  ([expr & exprs]
   (mapv snip-input (list* expr exprs))))

(defn keyed
  [k f]
  (fn [& args]
    [k (apply f args)]))

(defn val-mapped
  [m]
  (fn [& args]
    (zipmap (keys m) (map #(apply % args) (vals m)))))

(def transforms
  {:integer          #(Integer. %)
   :name             symbolify
   :namespace        symbolify
   :qualified-name   (fn
                       ([nm-part]         nm-part)
                       ([ns-part nm-part] (symbol (str ns-part) (str nm-part))))
   :keyword-name     keyword
   :cardinality      (fn [s] (keyword "db.cardinality" s))
   :kind             (fn [s] (keyword "db.type" s))
   :quotedstring     (fn [s] (apply str (drop 1 (butlast s))))
   :toggle           keyword
   :Verb             keyword
   :InterceptQueue   identity
   :InterceptorRef   vector
   :InterceptorRefs  vectorize
   :StockInterceptor identity
   :Respond          (fn [nm & clauses]
                       (lit/map->RespondAction (into {:name nm} clauses)))
   :Redirect         (fn [nm & clauses]
                       (lit/map->RedirectAction (into {:name nm} clauses)))
   :Interceptor      (fn [nm & clauses]
                       (lit/map->InterceptAction (into {:name nm} clauses)))
   :Conform          (fn [nm & clauses]
                       (lit/map->ConformAction (into {:name nm} clauses)))
   :Query            (fn [nm & clauses]
                       (lit/map->QueryAction (into {:name nm} clauses)))
   :params           (keyed :params vector)
   :edn-coerce       (keyed :edn-coerce vector)
   :headers          maplike
   :body             (keyed :body native)
   :url-literal      (keyed :url identity)
   :url-expr         (keyed :url native)
   :spec             (keyed :spec native)
   :q                (keyed :query native)
   :EnterClause      (keyed :enter native)
   :LeaveClause      (keyed :leave native)
   :ErrorClause      (keyed :error native)})

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
