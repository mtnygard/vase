(ns com.cognitect.vase.actions
  "Functions to construct interceptors dynamically.

  The functions with names that end in '-action' compile Pedestal
  interceptors. These are the main public entry points, and are used
  by the `literals` namespace when loading Vase descriptors.

  Take care to avoid code generation during request processing. It is
  time-consuming, so it should be done at application startup time
  instead.

  Actions are created by emitting code (in the functions named
  `-exprs`) which gets evaluated to build an interceptor. Arguments to
  the `-exprs` functions are interpolated into the emitted code. These
  arguments can contain literals or one s-expression each.

  For example, `respond-action-exprs` has an argument `body` that will
  be used as the HTTP response body. It may be a string or an
  s-expression.

  When s-expression arguments are evaluated, they have some bindings
  available in scope:

  - `request` - contains the Pedestal request map
  - `context` - contains the Pedestal context map
  - request parameters captured from path-params or parsed body
    parameters _and_ named in the `params` seq

  E.g., if `params` holds `['userid]` and the request matched a route
  with \"/users/:userid\", then the s-expression will have the symbol
  `'userid` bound to the value of that request parameter."
  (:require [clojure.walk :as walk]
            [clojure.spec.alpha :as s]
            [datomic.api :as d]
            [datomic.client.api :as client]
            [io.pedestal.interceptor :as i]
            [io.pedestal.interceptor.chain :as chain]
            [com.cognitect.vase.edn :as edn]
            [com.cognitect.vase.util :as util]
            [com.cognitect.vase.response :as response])
  (:import java.net.URLDecoder))

;; Code generation tools
;;
(defn decode-map
  "URL Decode the values of a Map
  This opens up the potential for non-sanitized input to be rendered."
  [m]
  (walk/postwalk
   #(cond-> %
      (string? %)
      URLDecoder/decode)
   m))

(defn coerce-arg-val
  ([v]
   (try
       (edn/read v)
       (catch Exception e v)))
  ([args k]
   (let [v (get args k)]
     (coerce-arg-val v)))
  ([args k default-v]
   (let [v (get args k default-v)]
     (coerce-arg-val v))))

(defn process-lookup-ref
  [r]
  (update r 0 keyword))

(defn process-id
  [entity-data]
  (cond-> entity-data
    (vector? (:db/id entity-data))
    (assoc :db/id (process-lookup-ref (:db/id entity-data)))))

(defn process-assert
  [args]
  (mapv process-id args))

(defn process-retract
  [args]
  (mapv #(vector :db.fn/retractEntity (:db/id %)) args))

(def db-ops
  {:vase/assert-entity  `process-assert
   :vase/retract-entity `process-retract})

(defn tx-processor
  [op]
  (if op
    (or (get db-ops op)
        (throw (ex-info (str "Unknown DB Op: " op) {:db-op op})))
    `process-assert))

(defn merged-parameters
  [request]
  {:post [(map? %)]}
  (let [{:keys [path-params params json-params edn-params]} request]
    (merge (if (empty? path-params) {} (decode-map path-params)) params json-params edn-params)))

(defmacro gensyms [& x]
  (mapv #(list `gensym (list `quote %)) x))

;; Building Interceptors
;;
(defn dynamic-interceptor
  "Build an interceptor/interceptor from a map of keys to
  expressions. The expressions will be evaluated and must evaluate to
  a function of 1 argument. At runtime the function will be called
  with a Pedestal context map."
  ([name exprs]
   (i/interceptor
    (merge
     {:name name}
     (util/map-vals eval exprs))))
  ([name _ exprs]
   (dynamic-interceptor name exprs)))

;; Auxiliary functions for interceptor internals
;;
(defn coerce-params
  [req-params coercions]
  (reduce (fn [params-acc k]
             ;; Params are all string values, which allows if-let to be safe to use
             ;; We can't use just `coerce-arg-value` because we don't want to add missing/not-found keys
             (if-let [v (get params-acc k)]
               (assoc params-acc k (coerce-arg-val v))
               params-acc))
           req-params
           coercions))

(defn bind
  [params]
  (let [param-keys (mapv #(if (vector? %) (first %) %) params)
        param-defaults (into {} (filter vector? params))]
    `{:keys ~(or param-keys [])
      :or ~param-defaults}))

;; Interceptors for common cases
;;
(defn respond-action-exprs
  "Return code for a Pedestal interceptor that will respond with a
  canned response. The same `body`, `status`, and `headers` arguments
  are returned for every HTTP request."
  [params edn-coerce body status headers]
  (assert (or (nil? headers) (map? headers)) (str "Headers should be a map. I got " headers))
  (let [[request context] (gensyms request context)
        coercions         (mapv util/ensure-keyword (or edn-coerce []))
        body              (or body "")
        status            (or status 200)]
    `(fn [{~request :request :as ~context}]
       (let [req-params#    (merged-parameters ~request)
             req-params#    (coerce-params req-params# ~coercions)
             ~(bind params) req-params#
             resp#          (response/response ~body ~headers ~status)]
         (assoc ~context :response resp#)))))

(comment
  (clojure.pprint/pprint
    (respond-action-exprs '[url-thing]
                          '[url-thing]
                          '(str "You said: " url-thing " which is a " (type url-thing))
                          200 {}))
  )

(defrecord RespondAction [name params edn-coerce body status headers doc]
  i/IntoInterceptor
  (-interceptor [_]
    (dynamic-interceptor
     name
     {:enter
      (respond-action-exprs params edn-coerce body status headers)

      :action-literal
      :vase/respond})))

(defmethod print-method RespondAction [t ^java.io.Writer w]
  (.write w (str "#vase/respond" (into {} t))))

(defn redirect-action-exprs
  "Return code for a Pedestal interceptor function that returns a
  redirect response."
  [params body status headers url]
  (assert (or (nil? headers) (map? headers)) (str "Headers should be a map. I got " headers))
  (let [[request context] (gensyms request context)
        body              (or body "")
        status            (or status 302)
        url               (or url "")]
    `(fn [{~request :request :as ~context}]
       (let [req-params#    (merged-parameters ~request)
             ~(bind params) req-params#
             resp#          (response/response
                              ~body
                              (merge ~headers {"Location" ~url})
                              ~status)]
         (assoc ~context :response resp#)))))

(defrecord RedirectAction [name params body status headers url]
  i/IntoInterceptor
  (-interceptor [_]
    (dynamic-interceptor
     name
     {:enter
      (redirect-action-exprs params body status headers url)

      :action-literal
      :vase/redirect})))

(defmethod print-method RedirectAction [t ^java.io.Writer w]
  (.write w (str "#vase/redirect" (into {} t))))

(defn detect-problems
  [spec params]
  (mapv #(dissoc % :pred)
    (:clojure.spec.alpha/problems
     (clojure.spec.alpha/explain-data spec params))))

(defn validate-action-exprs
  "Return code for a Pedestal interceptor function that performs
  clojure.spec.alpha validation on the parameters."
  [params headers spec request-params-path]
  (assert (or (nil? headers) (map? headers)) (str "Headers should be a map. I got " headers))
  (let [[request context] (gensyms request context)
        param-lookup      (if request-params-path
                            `(get-in ~request ~request-params-path)
                            `(merged-parameters ~request))]
    `(fn [{~request :request :as ~context}]
       (let [req-params#    ~param-lookup
             ~(bind params) req-params#
             problems#      (detect-problems ~spec req-params#)
             resp#          (response/response
                              problems#
                              ~headers
                              (response/status-code problems# (:errors ~context)))]
         (if (or (empty? (:io.pedestal.interceptor.chain/queue ~context))
               (seq problems#))
           (assoc ~context :response resp#)
           ~context)))))

(defrecord ValidateAction [name params headers spec request-params-path doc]
  i/IntoInterceptor
  (-interceptor [_]
   (dynamic-interceptor
     name
     {:enter
      (validate-action-exprs (or params []) headers spec request-params-path)

      :action-literal
      :vase/validate})))

(defmethod print-method ValidateAction [t ^java.io.Writer w]
  (.write w (str "#vase/validate" (into {} t))))

(defn- get-or-get-in
  [mapsym wheresym]
  (list (if (vector? wheresym) `get-in `get)
    mapsym wheresym))

(defn- assoc-or-assoc-in
  [mapsym wheresym valsym]
  (list (if (vector? wheresym) `assoc-in `assoc)
    mapsym wheresym valsym))

(defn- attach
  [m where val]
  (if (vector? where)
    (assoc-in m where val)
    (assoc    m where val)))

(defn conform-action-exprs
  "Return code for a Pedestal interceptor function that performs
  spec validation on the data attached at `from`. If the data
  does not conform, the explain-data will be attached at `explain-to`"
  [from spec to explain-to]
  (let [[request context conformed val] (gensyms request context conformed val)
        explain-to            (or explain-to ::explain-data)
        get-clause            (get-or-get-in     context from)
        assoc-data-clause     (assoc-or-assoc-in context to conformed)
        assoc-problems-clause (assoc-or-assoc-in context explain-to
                                `(clojure.spec.alpha/explain-data ~spec ~val))]
    `(fn [{~request :request :as ~context}]
       (let [~val       ~get-clause
             ~conformed (clojure.spec.alpha/conform ~spec ~val)
             ~context   ~assoc-data-clause]
         (if (clojure.spec.alpha/invalid? ~conformed)
           ~assoc-problems-clause
           ~context)))))

(defrecord ConformAction [name from spec to explain-to doc]
  i/IntoInterceptor
  (-interceptor [_]
    (dynamic-interceptor
     name
     {:enter
      (conform-action-exprs from spec to explain-to)

      :action-literal
      :vase/conform})))

(defmethod print-method ConformAction [t ^java.io.Writer w]
  (.write w (str "#vase/conform" (into {} t))))


(comment

  (conform-action-exprs [:context :request :param] :specname :to :explain-to)

  )

(defn hash-set? [x]
  (instance? java.util.HashSet x))

(defprotocol DatomicCodeGen
  (query-expr [this] "Return the correct code for issuing a query to this backend.")
  (transact-expr [this] "Return the correct code for running a transaction on this backend."))

(declare apply-tx apply-tx-cloud)

(defn peer-code-gen []
  (reify DatomicCodeGen
    (query-expr [this] `d/q)
    (transact-expr [this] `apply-tx)))

(defn- cloud-code-gen []
  (reify DatomicCodeGen
    (query-expr [this] `client/q)
    (transact-expr [this] `apply-tx-cloud)))

(defn query-action-exprs
  "Return code for a Pedestal interceptor function that performs a
  Datomic query.

  `query` holds a query expression in any supported Datomic
  format. Required.

  `variables` is a vector of the query variables (expressed as
  symbols) that should arrive in the Pedestal request map (as keywords).
  These will be supplied to the query as inputs. Values within the
  `variables` vector may also be pair-vectors, in the form `[sym-key default-value]`,
  allowing for default values if the key/keyword is not found in the request map.
  `variables` may be nil.

  `coercions` is a collection of variable names (expressed as symbols) that
  should be read as EDN values from the Pedestal request map. (I.e.,
  anything that needs to be converted from String to Date, Long, etc.)
  May be nil.

  `constants` is a vector of extra inputs to the query. These will be
  appended to the query inputs _after_ the variables. May be nil.

  `headers` is an expression that evaluates to a map of header
  name (string) to header value (string). May be nil."
  [code-gen query variables coercions constants headers to]
  (assert (or (nil? headers) (map? headers)) (str "Headers should be a map. I got " headers))
  (let [[request context args query-params response-body] (gensyms request context args query-params response-body)
        to                                                (or to ::query-data)
        coercions                                         (into #{} coercions)]
    `(fn [{~request :request :as ~context}]
       (let [~args            (merged-parameters ~request)
             vals#            ~(mapv
                                 (fn [x]
                                   (let
                                       [[k-sym default-v] (if (vector? x) x [x nil])
                                        k                 (util/ensure-keyword k-sym)]
                                     (if (contains? coercions k-sym)
                                       `(coerce-arg-val ~args ~k ~default-v)
                                       `(get ~args ~k ~default-v))))
                                 variables)
             ~query-params    (concat vals# ~constants)
             query-result#    (when (every? some? ~query-params)
                                (apply ~(query-expr code-gen) ~(list `quote query) (:db ~request) ~query-params))
             missing-params?# (not (every? some? ~query-params))
             ~response-body   (cond
                                missing-params?#          (str
                                                            "Missing required query parameters; One or more parameters was `nil`."
                                                            "  Got: " (keys ~args)
                                                            "  Required: " ~(mapv util/ensure-keyword variables))
                                (hash-set? query-result#) (into [] query-result#)
                                :else                     query-result#)
             resp#            (response/response
                                ~response-body
                                ~headers
                                (if query-result#
                                  (response/status-code ~response-body (:errors ~context))
                                  400))]
         (if (empty? (:io.pedestal.interceptor.chain/queue ~context))
           (assoc ~context :response resp#)
           ~(assoc-or-assoc-in context to response-body))))))

(comment

  (clojure.pprint/pprint
    (query-action-exprs
      (peer-code-gen)
      '[:find ?e
        :in $ ?someone ?fogus
        :where
        [(list ?someone ?fogus) [?emails ...]]
        [?e :user/userEmail ?emails]]
      '[[selector [*]]  someone]
      '[selector]
      ["mefogus@gmail.com"]
      {}
      nil))


  (eval  (query-action-exprs
           (peer-code-gen)
           '[:find ?e
             :in $ ?someone ?fogus
             :where
             [(list ?someone ?fogus) [?emails ...]]
             [?e :user/userEmail ?emails]]
           '[[selector [*]]
             someone]
           '[selector]
           ["mefogus@gmail.com"]
           {}
           nil))
  )

(def ^:private self (comp first ::chain/stack))
(def ^:private more-interceptors? (comp not empty? ::chain/queue))

(defn- apply-coercions
  [params vals coercions]
  (mapv
    (fn [x]
      (let
          [[k-sym default-v] (if (vector? x) x [x nil])
           k                 (util/ensure-keyword k-sym)]
        (if (contains? coercions k-sym)
          (coerce-arg-val vals k default-v)
          (get vals k default-v))))
    params))

(defn- query-action
  [context]
  (let [this            (self context)
        request         (:request context)
        args            (merged-parameters request)
        vals            (apply-coercions (:params this) args (set (:edn-coerce this)))
        query-params    (concat vals (:constants this))
        missing-params? (not (every? some? query-params))
        query-result    (when-not missing-params?
                          (apply (:query-fn this) (:query this) (:db request) query-params))
        response-body   (cond
                          missing-params?          (str
                                                     "Missing required query parameters; One or more parameters was `nil`."
                                                     "  Got: " (keys args)
                                                     "  Required: " (mapv util/ensure-keyword (:params this)))
                          (hash-set? query-result) (into [] query-result)
                          :else                    query-result)
        resp            (response/response
                          response-body
                          (:headers this)
                          (if query-result
                            (response/status-code response-body (:errors context))
                            400))]
    (if (more-interceptors? context)
      (attach context (:to this) response-body)
      (assoc context :response resp))))

(defrecord QueryAction [name params query edn-coerce constants headers to doc]
  i/IntoInterceptor
  (-interceptor [this]
    (i/interceptor
      {:name           name
       :params         params
       :query          query
       :edn-coerce     edn-coerce
       :constants      constants
       :headers        headers
       :to             to
       :doc            doc
       :enter          query-action
       :query-fn       d/q
       :action-literal :vase.datomic/query})))

(defmethod print-method QueryAction [t ^java.io.Writer w]
  (.write w (str "#vase.datomic/query" (into {} t))))

(defrecord CloudQueryAction [name params query edn-coerce constants headers to doc]
  i/IntoInterceptor
  (-interceptor [this]
    (dynamic-interceptor
     name
     {:enter
      (query-action-exprs (cloud-code-gen) query params (into #{} edn-coerce) constants headers to)

      :action-literal
      :vase.datomic.cloud/query})))

(defmethod print-method CloudQueryAction [t ^java.io.Writer w]
  (.write w (str "#vase.datomic.cloud/query" (into {} t))))

(def eav (juxt :e :a :v))

(defn apply-tx
  [conn tx-data args]
  {:whitelist
   args

   :transaction
   (map eav (:tx-data (deref (d/transact conn tx-data))))})

(defn apply-tx-cloud
  [conn tx-data args]
  {:whitelist
   args

   :transaction
   (map eav (:tx-data (client/transact conn {:tx-data tx-data})))})

(defn transact-action-exprs
  "Return code for a Pedestal context function that executes a
  transaction.

  `properties` is a collection of keywords that name Datomic
  attributes. When an HTTP request arrives, these keywords are matched
  with their parameter values in the request to form an entity map.

  `db-op` may be either :vase/assert-entity, :vase/retract-entity, or
  nil. When `nil`, Vase will assume the transaction body is a
  collection of Datomic entity maps.

  `headers` is an expression that evaluates to a map of header
  name (string) to header value (string). May be nil."
  [code-gen properties db-op headers to]
  (assert (or (nil? headers) (map? headers)) (str "Headers should be a map. I got " headers))
  (let [[request context response-body] (gensyms request context response-body)
        to                              (or to ::transact-data)
        processor                       (tx-processor db-op)
        tx-expr                         (transact-expr code-gen)]
    `(fn [{~request :request :as ~context}]
       (let [args#          (mapv
                              #(into {} (filter second (select-keys % ~(vec properties))))
                              (get-in ~request [:json-params :payload]))
             tx-data#       (~processor args#)
             conn#          (:conn ~request)
             ~response-body (~tx-expr conn# tx-data# args#)
             resp#          (response/response
                              ~response-body
                              ~headers
                              (response/status-code ~response-body (:errors ~context)))]
         (if (empty? (:io.pedestal.interceptor.chain/queue ~context))
           (assoc ~context :response resp#)
           (assoc-in
             ~(assoc-or-assoc-in context to response-body)
             [:request :db] (d/db conn#)))))))

(defrecord TransactAction [name properties db-op headers to doc]
  i/IntoInterceptor
  (-interceptor [_]
    (dynamic-interceptor
     name
     {:enter
      (transact-action-exprs (peer-code-gen) properties db-op headers to)

      :action-literal
      :vase.datomic/transact})))

(defmethod print-method TransactAction [t ^java.io.Writer w]
  (.write w (str "#vase.datomic/transact" (into {} t))))

(defrecord CloudTransactAction [name properties db-op headers to doc]
  i/IntoInterceptor
  (-interceptor [_]
    (dynamic-interceptor
     name
     {:enter
      (transact-action-exprs (cloud-code-gen) properties db-op headers to)

      :action-literal
      :vase.datomic.cloud/transact})))

(defmethod print-method CloudTransactAction [t ^java.io.Writer w]
  (.write w (str "#vase.datomic.cloud/transact" (into {} t))))

(defn- handle-intercept-option [x]
  (cond
    (list? x)   (eval x)
    (symbol? x) (resolve x)
    (var? x)    (deref x)
    :else       x))

(defrecord InterceptAction [name enter leave error]
  i/IntoInterceptor
  (-interceptor [_]
    (dynamic-interceptor
     name
     (cond-> {:action-literal :vase/intercept}
       enter (assoc :enter (handle-intercept-option enter))
       leave (assoc :leave (handle-intercept-option leave))
       error (assoc :error (handle-intercept-option error))))))

(defmethod print-method InterceptAction [t ^java.io.Writer w]
  (.write w (str "#vase/intercept" (into {} t))))

(defn- attach-action-exprs
  [key val]
  (let [context (gensym 'context)]
    `(fn [~context]
       ~(assoc-or-assoc-in context key val))))

(defrecord AttachAction [name key val]
  i/IntoInterceptor
  (-interceptor [_]
    (dynamic-interceptor
     name
     {:enter
      (attach-action-exprs key val)
      :action-literal
      :vase/attach})))

(defmethod print-method AttachAction [t ^java.io.Writer w]
  (.write w (str "#vase/attach" (into {} t))))
