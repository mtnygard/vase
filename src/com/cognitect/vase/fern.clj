(ns com.cognitect.vase.fern
  (:require [clojure.spec :as s]
            [clojure.string :as str]
            [com.cognitect.vase :as vase]
            [com.cognitect.vase.literals :as lit]
            [instaparse.core :as insta]
            [instaparse.gll :as gll]
            [instaparse.failure :as insta-failure]
            [io.pedestal.http :as http]))

(defn- map-keys
  [f m]
  (with-meta (zipmap (map f (keys m)) (vals m)) (meta m)))

(defn- map-vals
  [f m]
  (with-meta (zipmap (keys m) (map f (vals m))) (meta m)))

(defn- renamespace
  "Move a key from one namespace to another. `nm` may be a symbol or
  nil. If nil, the new keyword has no namespace part."
  ([k]
   (renamespace k nil))
  ([k nm]
   {:pre [(keyword? k) (or (symbol? nm) (nil? nm))]}
   (if nm
     (keyword (str nm) (name k))
     (keyword (name k)))))

;; ========================================
;; Markers communicate where there are problems in the input.

(declare source-text)

(defn marker? [v]
  (and (map? v) (contains? v :marker-text)))

(defn- pprint-failure
  [{:keys [line column text]}]
  (str/join \newline
            [(str "Parse error at line " line ", column " column ":")
             text
             (insta-failure/marker column)]))

(defn- insta-failure->marker
  [f]
  (assoc (map-keys renamespace f)
         :marker-text  (pprint-failure f)
         :source-text  (:text f)))

(defn node->marker
  [node marker-text]
  (let [m (meta node)]
    (assoc (map-keys renamespace m)
           :marker-text marker-text
           :source-text (source-text node))))

(defn exception->marker
  [node exception]
  (node->marker node (.getMessage exception)))

(defn attach-marker
  [context marker]
  (update context :markers #(conj (or % []) marker)))

(defn attach-marker-about
  [context node s]
  (attach-marker context (node->marker node s)))

;; ========================================
;; Native expressions get clipped out of the original input, after we
;; use the parser to figure out where the native expressions actually
;; are.

(def ^:private ^:dynamic *input* nil)

(defn- source-text
  [node]
  (when-let [[start end] (insta/span node)]
    (subs *input* (inc start) end)))

(defn- read-expr
  [node]
  (try
    (read-string (source-text node))
    (catch Exception ex
        (exception->marker node ex))))

(defn- snip-input
  [node]
  (when-let [[start end] (insta/span node)]
    (read-string (subs *input* (inc start) end))))

;; ========================================
;; One level of parser eliminates comments and whitespace

(def ^:private whitespace-or-comments
  (insta/parser
   "ws-or-comments = #'\\s+' | comments
    comments = comment+
    comment = ';' #'.*?(\\r\\n|\\n|\\z)'"
   :auto-whitespace :standard))

;; ========================================
;; The next level of parser splits the input into "blocks" which each
;; start with a keyword and end with the string 'end'.

(def parsed-blocks
  (insta/parser
   "File = Block+
    Block = blocktype body
    end = <'end'>

    blocktype = #'[A-Za-z_][A-Za-z0-9_\\-\\?!$\\%]*'
    body = (native-expr | body-token)* end
    native-expr = #\"<<.*?>>\"

    <body-token> =  !('end' | '<<' | '>>') (list-expr | vec-expr | map-expr | word | quotedstring)
    list-expr = <'('> (body-token / word)* <')'>
    vec-expr = <'['> (body-token / word)* <']'>
    map-expr = <'{'> (body-token / word)* <'}'>
    word = #\"[#a-zA-Z?'.:_\\-0-9*+$&%^.,/\\\"<>=]+\"
    quotedstring = #'\"[^\"]*\"'" :auto-whitespace whitespace-or-comments))

(def ^:private block-transformations
  {:native-expr (fn [s] [:native-expr (read-string (subs s 2 (- (count s) 2)))])
   :blocktype   (fn [t] (merge {:tag t} (meta t)))
   :body        (fn [& phrases] (butlast phrases))
   :Block       (fn
                  ([t]   {:type t})
                  ([t b] {:type t :body b}))})

(def transformed-blocks (partial insta/transform block-transformations))

;; ========================================
;; Each block is processed (after parsing) separately. Processors
;; dispatch on the initial keyword via a multimethod. New block types
;; can be introduced by adding methods.

(defn block-type [b] (get-in b [:type :tag]))

(defmulti process-block (fn [context input] (block-type input)))

(defmethod process-block :default
  [context input]
  (attach-marker-about context (:type input) (str "I don't know what to do with the keyword '" (block-type input) "'")))

(defn process-blocks
  [context input]
  {:pre [(map? context) (vector? input) (= :File (first input))]}
  (reduce process-block context (rest input)))

(def initial-context {})

(defn parse-and-process
  [input]
  (binding [*input* input]
    (let [ast (parsed-blocks input)]
      (if-let [failure (insta/get-failure ast)]
        (insta-failure->marker failure)
        (let [ast (transformed-blocks (insta/add-line-and-column-info-to-metadata input ast))]
          (process-blocks initial-context ast))))))

;; ========================================
;; These are API functions that block processors can use to make their
;; jobs easier.


(defn guard*
  ([final] final)
  ([guard binding else & more]
   `(if ~guard
      (let ~binding
        ~(apply guard* more))
      ~else)))

(defmacro guarded->
  "[[guard binding else]* final-expr]

  Apply a series of tests and transformations, with an exit value
  allowed at each point.  Each guard must be a single expression. All
  previous bindings are in scope for following guards. A binding takes
  the same form as a let-binding.

  If a guard fails, the whole expression will evaluate to the `else`
  expression for that guard. All previous bindings are in scope for
  the else, but _not_ the binding whose guard failed.

  Once all guards and bindings are evaluated, `final-expr` will be
  evaluated and its value returned."
  [& forms] (apply guard* forms))

(defn keyword-named-block
  "Extract a keyword name from a block. The name must be the second
  thing in the block (following the blocktype tag) and must be a :word
  element from the block parser.

  Returns either a keyword or a marker."
  [block]
  (let [kind (block-type block)]
    (guarded->
     (not (empty? (:body block)))
     [body (:body block)]
     (node->marker block (str "A " kind " block needs a name after the '" kind "' keyword"))

     (seq? body)
     [word (first body)]
     (node->marker body (str "A " kind "'s body must have a name"))

     (= :word (first word))
     [nm (second word)]
     (node->marker word (str "The name of a " kind " block must resemble a Clojure keyword without the leading ':'"))

     (keyword nm))))

(defn body->keyword-map
  [parts where allowed-keyword? describe-allowed]
  (loop [result  {}
         markers []
         parts   parts]
    (if (seq parts)
      (let [[[_ k :as w1] w2] (take 2 parts)]
        (if (nil? w2)
          (recur result (conj markers (node->marker w1 "There was an extra keyword with no value.")) (drop 2 parts))
          (if (allowed-keyword? (keyword k))
            (let [v (read-expr w2)]
              (if (marker? v)
                (recur result (conj markers v) (drop 2 parts))
                (recur (assoc result (keyword k) v) markers (drop 2 parts))))
            (recur result (node->marker w1  (str "An " where " block doesn't allow " k " as a keyword. I'm ignoring it.")) (drop 2 parts)))))
      [result markers])))

;; ========================================
;; Handle the builtin 'http' block

(def ^:private http-keywords
  #{:allowed-origins :router :resource-path
    :method-param-name :secure-headers :type
    :port :host :enable-session :enable-csrf
    :file-path :interceptors :container-options
    :not-found-interceptor :mime-types})

(defmethod process-block "http"
  [context input]
  (let [[result markers] (body->keyword-map (:body input) "http" http-keywords (str "Allowed keywords are " (str/join ", " (map name http-keywords))))]
    (cond-> (assoc context :vase/service-map result)
      (seq markers)
      (update :markers into markers))))

;; ========================================
;; Handle the builtin 'schema' block

(def ^:private schema-keywords
  #{:attr})

(defn- schema-attr
  [input]
  (take-while #(not= :quotedstring (first %)) input))

(defmethod process-block "schema"
  [context input]
  (let [name-or-marker (keyword-named-block input)]
    (guarded->
     (not (marker? name-or-marker))
     [nm name-or-marker]
     (attach-marker context name-or-marker)

     (seq? (rest (:body input)))
     [remainder (rest (:body input))]
     (assoc context :vase/norms {nm {}})

     (doto remainder println)
     )
))

;; ========================================
;; From here down is the original version of the parser. It's here for
;; reference and to keep the tests passing until I finish migrating to
;; the new design.


(def ^:private fern-parser
  (insta/parser
   "Description = (Http | Schema | Api | Spec | StockInterceptor )*
    Http = <'http'> HttpClause*
    HttpClause = HttpKeyword NativeExpr
    HttpKeyword = 'allowed-origins' | 'container-options' | 'enable-csrf' | 'enable-session' | 'file-path' | 'host' | 'interceptors' | 'method-param-name' | 'mime-types' | 'not-found-interceptor' | 'port' | 'resource-path' | 'router' | 'secure-headers' | 'type'
    NativeExpr = s-expr

    Schema = <'schema'> keyword-name (Attribute | Require)*

    Require = <'require'> keyword-name
    Attribute = <'attribute'> keyword-name cardinality kind toggle* quotedstring
    cardinality = 'one' | 'many'
    kind = 'long' | 'double' | 'instant' | 'ref' | 'bigint' | 'float' | 'string' | 'keyword' | 'bigdec' | 'bytes' | 'uri' | 'uuid' | 'boolean'
    toggle = 'unique' | 'identity' | 'index' | 'fulltext' | 'component' | 'no-history'

    Api = <'api'> keyword-name (Route / Require)*
    Route = Verb Path InterceptQueue
    Verb = 'get' | 'put' | 'post' | 'delete' | 'head' | 'options'
    <Path> = quotedstring
    InterceptQueue = InterceptorRef | InterceptorRefs
    InterceptorRef = qualified-name
    InterceptorRefs = <'['> qualified-name+ <']'>

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
    <QueryClause> = to | headers | q | params | edn-coerce | constants
    q = <'q'> NativeExpr
    constants = <'constants'> NativeExpr

    Transact = <'transact'> keyword-name TransactClause*
    <TransactClause> = to | headers | properties | operation
    operation = <'operation'> keyword-name
    properties = <'properties'> <'['> keyword-name+ <']'>

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
  (when-let [[start end] (insta/span node)]
    (read-string (subs *input* (inc start) end))))

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

(def ^:private transforms
  {:integer            #(Integer. %)
   :name               symbolify
   :namespace          symbolify
   :qualified-name     (fn
                         ([nm-part]         nm-part)
                         ([ns-part nm-part] (symbol (str ns-part) (str nm-part))))
   :keyword-name       keyword
   :quotedstring       (fn [s] (apply str (drop 1 (butlast s))))
   :Attribute          (keyed :attribute build-attribute)
   :cardinality        keyword
   :kind               keyword
   :toggle             keyword
   :Http               (fn [& parts]
                         {:fern/http (reduce merge {} parts)})
   :Schema             (fn [nm & parts]
                         (let [{:keys [attribute require]} (group-by first parts)]
                           {:vase/norms
                            {nm
                             (cond-> {}
                               (seq require)
                               (assoc :vase.norm/requires (into [] (map second require)))

                               (seq attribute)
                               (assoc :vase.norm/txes (mapv second attribute)))}}))
   :Api                (fn [nm & parts]
                         (let [{:keys [route schema require]} (group-by first parts)]
                           {:vase/apis
                            {nm
                             (cond-> {}
                               (seq schema)
                               (assoc :vase.api/schemas (into [] (map second schema)))

                               (seq require)
                               (update :vase.api/schemas #(into (or % []) (mapv second require)))

                               (seq route)
                               (assoc :vase.api/routes (apply merge-with merge (map second route))))}}))
   :Spec               hash-map
   :Require            (keyed :require identity)
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
                         (lit/map->TransactAction (into {:name nm} clauses)))
   :HttpClause         hash-map
   :HttpKeyword        (fn [kw] (keyword "io.pedestal.http" kw))
   :NativeExpr         native
   :params             (keyed :params vector)
   :param-with-default (fn [nm default] (vector nm (native default)))
   :properties         (keyed :properties vector)
   :edn-coerce         (keyed :edn-coerce vector)
   :headers            (keyed :headers maplike)
   :body               (keyed :body identity)
   :with-spec          (keyed :spec identity)
   :q                  (keyed :query identity)
   :InterceptorKeyword keyword
   :InterceptorClause  hash-map})

(defn- transform
  [parse-tree]
  (if (insta/failure? parse-tree)
    parse-tree
    (insta/transform transforms parse-tree)))


;;; ========================================
;;; Syntax helpers

(declare marker?)

(defn ok? [v] (not (and (map? v) (marker? v))))

(defmacro ok?->
  "Binds name to expr, checks if it is an error. If not, evaluates the
  first form in the lexical context of that binding, then checks if
  that evaluates to an error. Repeats for each successive form,
  returning either the first error or the result of the last form."
  [expr name & forms]
  (let [steps (map (fn [step] `(if (ok? ~name) (-> ~name ~step) ~name)) forms)]
    `(let [~name ~expr
           ~@(interleave (repeat name) (butlast steps))]
       ~(if (empty? steps)
          name
          (last steps)))))

;;; ========================================
;;; Assembling the Vase descriptor

(defn- sanity-checks
  [ast]
  ast)

(defn- attach-pedestal-service-map
  [ast]
  (def ast* ast)
  (let [{:keys [vase/apis vase/specs vase/norms]} (next ast)]
    {:vase/apis (merge-with merge apis)
     :vase/specs (merge-with merge specs)
     :vase/norms (merge-with merge norms)}
    )
  )

;;; ========================================
;;; Public API

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
   (let [ast (binding [*input* content]
               (transform
                (insta/parse fern-parser content :start start :total true)))]
     (if-let [failure (insta/get-failure ast)]
       (insta-failure->marker failure)
       ast))))

(defn load-descriptor
  "Load the contents of the file located at `fern-path`. Returns a
  Pedestal service map with the Vase descriptor already attached
  as :com.cognitect.vase/descriptor. The service map will be
  initialized but not started."
  [fern-path]
  (ok?-> fern-path service
         slurp
         (parse-string)
         (sanity-checks)
         (attach-pedestal-service-map))


  )
