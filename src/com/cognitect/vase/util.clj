(ns com.cognitect.vase.util
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as cstr]
            [cheshire.core :as json]
            [cognitect.transit :as transit])
  (:import (java.io ByteArrayInputStream
                    FileInputStream
                    File)
           (javax.xml.bind DatatypeConverter)))

(defn map-vals
  [f m]
  (reduce-kv (fn [m k v] (assoc m k (f v))) m m))

(defn str->inputstream
  ([^String text]
   (str->inputstream text "UTF-8"))
  ([^String text ^String encoding]
   (ByteArrayInputStream. (.getBytes text encoding))))

(defn short-hash []
  (subs
    (DatatypeConverter/printBase64Binary
      (byte-array (loop [i 0
                         ret (transient [])]
                    (if (< i 8)
                      (recur (inc i) (conj! ret (.byteValue ^Long (long (rand 100)))))
                      (persistent! ret)))))
    0 11))

;; This function is useful when writing your own action literals,
;; allowing you to expand symbol names within the descriptors.
;; It's not used within the Vase source, but has been used on projects
;; built with Vase.
(defn fully-qualify-symbol
  ([sym] (fully-qualify-symbol *ns* sym))
  ([ns sym]
     (if-let [ns-alias? (namespace sym)]
       (let [ns-aliases (ns-aliases ns)]
         (if-let [fqns (ns-aliases (symbol ns-alias?))]
           (symbol (name (ns-name fqns)) (name sym))
           sym))
       sym)))

(defn ensure-keyword [x]
    (cond
      (keyword? x) x
      (string? x) (keyword x)
      (symbol? x) (keyword (namespace x) (name x))
      :else (keyword (str x))))

(def write-edn pr-str)

(defn read-edn
  "Converts an edn string into Clojure data. `args` are clojure.edn `opts`
  `:readers` defaults to `*data-readers*`"
  [string & args]
  (let [e (edn/read-string (merge {:readers *data-readers*}
                                  (apply hash-map args))
                           string)]
    (if (instance? clojure.lang.IObj e)
      (with-meta e {:vase/src string})
      e)))

(defn edn-resource
  "Load an EDN resource file and read its contents. The only required argument
  is `file-path`, which is the path of a file relative the projects resources
  directory (`resources/` or, for tests, `test/resources/`).

  Optional arguments:

  * `fallback-path` - A \"default\" path to check if file-path is actually an
    empty string. Useful in places you load a `file-path` from a config and its
    value might be absent.
  * `process-path-fn` - The function to use for getting the URL of the file. By
    default uses `clojure.java.io/resource`."
  ([file-path]
   (edn-resource file-path "" io/resource))
  ([file-path fallback-path]
   (edn-resource file-path fallback-path io/resource))
  ([file-path fallback-path process-path-fn]
   (let [trimmed-path (or (not-empty (cstr/trim file-path))
                          (not-empty (cstr/trim fallback-path)))
         contents (some->>
                    trimmed-path
                    process-path-fn
                    slurp)]
     (if contents
       (read-edn contents)
       (throw (ex-info
                (str "Failed to read an EDN file: " file-path " :: trimmed to: " trimmed-path)
                {:file-path file-path
                 :trimmed-path trimmed-path}))))))

(defn edn-file
  [file-path]
  (edn-resource file-path "" (fn [^String x] (io/as-url (File. x)))))

(defn read-json
  "Converts json string to Clojure data. By default, keys are keywordized."
  [string & args]
  (apply json/parse-string string keyword args))

(defn write-json
  "Writes json string given Clojure data. By default, unicode is not escaped."
  [data & args]
  (json/generate-string data (apply hash-map args)))

(defn read-transit-json
  [transit-json-str]
  (-> transit-json-str
      str->inputstream
      (transit/reader :json)
      transit/read))

;; Response Generation
;; -------------------
(defn- complete-with-errors?
  [response errors]
  (and (not (nil? response)) (seq errors)))

(defn- bad-request?
  [response errors]
  (and (nil? response) (seq errors)))

(defn- exception?
  [response]
  (instance? Throwable response))

(defn status-code
  [response errors]
  (cond
    (complete-with-errors? response errors) 205
    (bad-request?          response errors) 400
    (exception?            response)        500
    :else                                   200))

(defn response
  [body headers status]
  {:body    (or body "")
   :headers (or headers {})
   :status  (or status 200)})

(defn payload-response
  ([request response-data errors-data]
   (payload-response request response-data errors-data))
  ([request response-data errors-data headers]
   (payload-response request response-data errors-data headers (status-code response-data errors-data)))
  ([request response-data errors-data headers status]
   (response (merge {:request {:body (get request :json-params
                                          (get request :edn-params
                                               (get request :params
                                                    (get request :body-string ""))))
                               :this_ (:uri request)
                               :doc (:vase-doc request "")
                               :server_received_time (str (:received-time request))}}
                    (when (seq response-data)
                      {:response response-data})
                    (when (seq errors-data)
                      {:errors errors-data}))
             headers
             status)))

(defn empty-value [[_ _ v]] (or (nil? v) (and (coll? v) (empty? v))))
(defn eseq? [v] (and (sequential? v) (every? map? v)))

(defn emap->datoms
  [idx e emap]
  (mapcat
   (fn [[a v]]
     (cond (eseq? v)
           (mapcat
            #(let [next-id (swap! idx inc)]
               (list*
                [e a next-id]
                (emap->datoms idx next-id %)))
            v)

           (and (sequential? v) (not (sequential? (first v))))
           (map #(vector e a %) v)

           :else
           [[e a v]]))
   emap))

(defn push-down-names [m]
  (mapv (fn [[k v]] (assoc v :vase/name k)) m))

(defn name-value-entities [m val-key]
  (mapv (fn [[n v]] {:vase/name n val-key v}) m))
