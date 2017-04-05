(ns com.cognitect.vase.fern-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer :all]
            [com.cognitect.vase.fern :as fern]
            [com.cognitect.vase.literals :as lit]
            [instaparse.core :as insta]
            [io.pedestal.http :as http]
            [io.pedestal.interceptor :as i]
            [clojure.set :as set]))

(defn map-vals
  [f m]
  (with-meta
    (reduce-kv
     #(assoc %1 %2 (f %3))
     {}
     m)
    (meta m)))

(defn- remove-tempids
  [entities]
  (map (fn [entity] (dissoc entity :db/id)) entities))

(defn- remove-tempids-from-norm
  [norm]
  (cond-> norm
    (:vase.norm/txes norm)
    (update :vase.norm/txes remove-tempids)))

(defn- remove-tempids-from-norms
  [norms]
  (map-vals remove-tempids-from-norm norms))

(defn- remove-tempids-from-nested-schema
  [api]
  (cond-> api
    (:fern.api/schema api)
    (update :fern.api/schema #(map-vals remove-tempids-from-norm %))))

(deftest test-block-parser
  (testing "names"
    (are [rule input expected] (= expected (fern/parsed-blocks input :start rule))
      :word        "example"                                [:word "example"]
      :word        "example-thing"                          [:word "example-thing"]
      :list-expr   "(1 2)"                                  [:list-expr [:word "1"] [:word "2"]]
      :list-expr   "()"                                     [:list-expr]
      :map-expr    "{:a 1 :b 2}"                            [:map-expr [:word ":a"] [:word "1"] [:word ":b"] [:word "2"]]
      :map-expr    "{}"                                     [:map-expr]
      :vec-expr    "[1 2]"                                  [:vec-expr [:word "1"] [:word "2"]]
      :vec-expr    "[]"                                     [:vec-expr]
      :list-expr   "([1 2] {:a 1})"                         [:list-expr [:vec-expr [:word "1"] [:word "2"]] [:map-expr [:word ":a"] [:word "1"]]]
      :native-expr "<<foo>>"                                [:native-expr "<<foo>>"]
      :Block       "foo end"                                [:Block [:blocktype "foo"] [:body [:end]]]
      :File        "foo end bar end"                        [:File [:Block [:blocktype "foo"] [:body [:end]]] [:Block [:blocktype "bar"] [:body [:end]]]]
      :Block       "foo zort quux end"                      [:Block [:blocktype "foo"] [:body [:word "zort"] [:word "quux"] [:end]]]
      :Block       "foo zort 8080 quux <<(fn [_] 0)>> end"  [:Block [:blocktype "foo"] [:body [:word "zort"] [:word "8080"] [:word "quux"] [:native-expr "<<(fn [_] 0)>>"] [:end]]]
      :Block       "foo zort <<(fn [_] end)>> end"          [:Block [:blocktype "foo"] [:body [:word "zort"] [:native-expr "<<(fn [_] end)>>"] [:end]]])))

(deftest test-block-transformer
  (are [input expected] (= expected (fern/transformed-blocks input))
    [:native-expr "<<(fn [_] end)>>"]
    [:native-expr '(fn [_] end)]

    [:Block [:blocktype "foo"]]
    {:type {:tag "foo"}}

    [:Block [:blocktype "foo"] [:body [:word "zort"] [:word "quux"] [:end]]]
    {:type {:tag "foo"}
     :body [[:word "zort"] [:word "quux"]]}

    [:Block [:blocktype "foo"] [:body [:native-expr "<<(fn [ctx] (map (fn [v] (inc v)) (:vals ctx)))>>"] [:end]]]
    {:type {:tag "foo"}
     :body [[:native-expr '(fn [ctx] (map (fn [v] (inc v)) (:vals ctx)))]]}))

(deftest test-block-processor-invocation
  (let [resulting-ctx (fern/parse-and-process "foo end bar end baz end")]
    (is (contains? resulting-ctx :markers))
    (is (= 3 (count (:markers resulting-ctx))))))

(defn- difference [a b]
  (let [projection (select-keys b (keys a))]
    (reduce-kv
     (fn [m k v]
       (cond
         (= v (get m k))  (dissoc m k)
         (not (contains? m k)) (assoc m k :missing)
         :else m))
     projection a)))

(defn- discrepancies
  [coll1 coll2]
  (remove empty?
        (map difference coll1 coll2)))

(deftest test-http-block
  (testing "happy path"
    (are [input expected] (= expected (get (fern/parse-and-process input) :vase/service-map))
      "http port 8080 end"               {:port 8080}
      "http port 49152 end"              {:port 49152}
      "http host \"my.example.com\" end" {:host "my.example.com"}
      "http method-param-name \"_verb\" resource-path \"public\" end" {:method-param-name "_verb" :resource-path "public"}))
  (testing "rejections"
    (are [input expected-markers] (= [] (discrepancies expected-markers (get (fern/parse-and-process input) :markers)))
      "http no-such-kw end"              [{:start-line 1
                                           :start-column 5
                                           :end-line 1
                                           :end-column 16
                                           :source-text "no-such-kw"}]
      "http port end"                    [{:start-line 1
                                           :start-column 5
                                           :end-line 1
                                           :end-column 10
                                           :source-text "port"}]
      "http port 8080+9090 end"          [{:start-line 1
                                           :start-column 10
                                           :end-line 1
                                           :end-column 20
                                           :source-text "8080+9090"}])))

(deftest test-parse-fern
  (testing "names"
    (are [input expected] (= expected (fern/parse-string input :qualified-name))
      "example"              'example
      "example.part"         'example.part
      "ns1.ns2/example.part" 'ns1.ns2/example.part
      "question?"            'question?
      "imperative!"          'imperative!
      "numeric001"           'numeric001
      "hyphen-split"         'hyphen-split
      "inner$classlike"      'inner$classlike
      "a100%"                'a100%))

  (testing "params"
    (are [input expected] (= expected (fern/parse-string input :params))
      "params [id1]"
      [:params [:id1]]

      "params [ns/id1]"
      [:params [:ns/id1]]

      "params [id1 id2 id3]"
      [:params [:id1 :id2 :id3]]

      "params [[with-default 0] no-default [string-default \"s\"]]"
      [:params [[:with-default 0] :no-default [:string-default "s"]]]))

  (testing "schema fragments"
    (are [input expected] (= expected (map-vals remove-tempids-from-norms (fern/parse-string input :Schema)))
      "schema example/base"
      {:vase/norms
       {:example/base {}}}

      "schema example/base
         attribute user/name one string \"This is a doc\""
      {:vase/norms
       {:example/base
        {:vase.norm/txes
         [{:db/ident              :user/name
           :db/valueType          :db.type/string
           :db/cardinality        :db.cardinality/one
           :db.install/_attribute :db.part/db
           :db/doc                "This is a doc"}]}}}

      "schema with-toggles
       attribute people.user/username one string identity fulltext no-history \"An attribute with toggles\""
      {:vase/norms
       {:with-toggles
        {:vase.norm/txes
         [{:db/index              true
           :db/unique             :db.unique/identity
           :db/valueType          :db.type/string
           :db/noHistory          true
           :db.install/_attribute :db.part/db
           :db/fulltext           true
           :db/cardinality        :db.cardinality/one
           :db/doc                "An attribute with toggles"
           :db/ident              :people.user/username}]}}}

      "schema multiple-attributes
         attribute user/id one long \"Numeric ID\"
         attribute user/name one string \"Printable\"
         attribute user/friends many ref \"Connections\""
      {:vase/norms
       {:multiple-attributes
        {:vase.norm/txes
         [{:db/ident              :user/id
           :db/valueType          :db.type/long
           :db/cardinality        :db.cardinality/one
           :db.install/_attribute :db.part/db
           :db/doc                "Numeric ID"}
          {:db/ident              :user/name
           :db/valueType          :db.type/string
           :db/cardinality        :db.cardinality/one
           :db.install/_attribute :db.part/db
           :db/doc                "Printable"}
          {:db/ident              :user/friends
           :db/valueType          :db.type/ref
           :db/cardinality        :db.cardinality/many
           :db.install/_attribute :db.part/db
           :db/doc                "Connections"}]}}}))

  (testing "api fragments"
    (are [input expected] (= expected (fern/parse-string input :Api))
      "api example/user"
      {:vase/apis
       {:example/user {}}}

      "api example/one-route
        get \"/foo\"  one-interceptor"
      {:vase/apis
       {:example/one-route
        {:vase.api/routes
         {"/foo" {:get ['one-interceptor]}}}}}

      "api example/one-route
        get \"/foo\"  [an-interceptor another-interceptor]"
      {:vase/apis
       {:example/one-route
        {:vase.api/routes
         {"/foo" {:get ['an-interceptor 'another-interceptor]}}}}}

      "api example/verbs
        get  \"/foo\" one-action
        put  \"/foo\" different-action
        post \"/foo\" other-action"
      {:vase/apis
       {:example/verbs
        {:vase.api/routes
         {"/foo" {:get ['one-action]
                  :put ['different-action]
                  :post ['other-action]}}}}}

      "api multiple/routes
        get \"/users\" list-users
        post \"/users\" create-user
        get \"/users/:id\" show-user
        delete \"/users/:id\" delete-user"
      {:vase/apis
       {:multiple/routes
        {:vase.api/routes
         {"/users"     {:get  ['list-users]
                        :post ['create-user]}
          "/users/:id" {:get    ['show-user]
                        :delete ['delete-user]}}}}}))

  (testing "stock interceptors"
    (are [input expected] (= expected (fern/parse-string input :StockInterceptor))
      "respond home-page
         status 200"
      (lit/map->RespondAction {:name :home-page :status 200})

      "respond home-page
         status 209
         params [query lang]
         headers {\"Produced-by\" \"TDD\"
                  \"Yielding\" \"100%\"}"
      (lit/map->RespondAction {:name :home-page :status 209 :params [:query :lang] :headers {"Produced-by" "TDD" "Yielding" "100%"}})

      "respond my-app/page1
         status 200
         params [id]
         edn-coerce [id]
         body (str \"You gave me ID \" id)"
      (lit/map->RespondAction {:name :my-app/page1 :status 200 :params [:id] :edn-coerce [:id] :body '(str "You gave me ID " id)})

      "respond my-app/add1
         params [id]
         edn-coerce [id]
         body (str \"One more is \" (inc id))"
      (lit/map->RespondAction {:name :my-app/add1 :params [:id] :edn-coerce [:id] :body '(str "One more is " (inc id))})

      ;; Can't use anonymous fn body in the test b/c it turns into a
      ;; function value, which we can't compare for equality.
      ;; Anonymous fns work just fine in real usage.
      "respond my-app/complex-body
         body (filter (fn [x] (< 100 %)) (load-scores))"
      (lit/map->RespondAction {:name :my-app/complex-body :body '(filter (fn [x] (< 100 %)) (load-scores))})

      "redirect aname
        status 302
        url \"http://www.google.com/\""
      (lit/map->RedirectAction {:name :aname :status 302 :url "http://www.google.com/"})

      "redirect userpage
         status 302
         headers {\"Extra-Header\" \"Indeed\" \"And-Another?\" \"Thank You\"}
         params [id]
         url (str \"/users/\" id)"
      (lit/map->RedirectAction {:name :userpage
                                :status 302
                                :params [:id]
                                :url '(str "/users/" id)
                                :headers {"Extra-Header" "Indeed" "And-Another?" "Thank You"}})

      "interceptor direct-code
         enter (fn [ctx] :enter)
         leave (fn [ctx] :leave)
         error (fn [ctx] :error)"
      (lit/map->InterceptAction {:name :direct-code :enter '(fn [ctx] :enter) :leave '(fn [ctx] :leave) :error '(fn [ctx] :error)})

      "conform aname
         from this-key
         with-spec (clojure.spec/and string? not-empty)
         to   that-key"
      (lit/map->ConformAction {:name :aname :from :this-key :to :that-key :spec '(clojure.spec/and string? not-empty)})

      "query single-result
         q      [:find ?email ?name :in $ ?id :where [?e :user/email ?email] [?e :user/id ?id]]
         params [id]
         to     result-key"
      (lit/map->QueryAction {:name   :single-result
                             :query  '[:find ?email ?name :in $ ?id :where [?e :user/email ?email] [?e :user/id ?id]]
                             :params [:id]
                             :to     :result-key})

      "query binding-form
         q [:find ?release-name
            :in $ ?artist-name
            :where [?artist :artist/name ?artist-name]
                   [?release :release/artists ?artist]
                   [?release :release/name ?release-name]]
         params [artist-name]"
      (lit/map->QueryAction {:name   :binding-form
                             :params [:artist-name]
                             :query  '[:find ?release-name
                                       :in $ ?artist-name
                                       :where [?artist :artist/name ?artist-name]
                                       [?release :release/artists ?artist]
                                       [?release :release/name ?release-name]]})

      "query scalar-result
         q      [:find ?year . :in $ ?name :where [?artist :artist/name ?name] [?artist :artist/startYear ?year]]
         params [name]"
      (lit/map->QueryAction {:name   :scalar-result
                             :params [:name]
                             :query  '[:find ?year . :in $ ?name :where [?artist :artist/name ?name] [?artist :artist/startYear ?year]]})

      "query params-differ-from-lvars
         q     [:find  ?release
                :in    $ [?aname ?rname]
                :where [?artist :artist/name ?aname]
                       [?release :release/artists ?artist]
                       [?release :release/name ?rname]]
         params [artist-release-pairs]"
      (lit/map->QueryAction {:name   :params-differ-from-lvars
                             :query  '[:find  ?release
                                       :in    $ [?aname ?rname]
                                       :where [?artist :artist/name ?aname]
                                       [?release :release/artists ?artist]
                                       [?release :release/name ?rname]]
                             :params [:artist-release-pairs]})

      "query with-everything
         q     [:find  ?release
                :in    $ [?aname ?rname]
                :where [?artist :artist/name ?aname]
                       [?release :release/artists ?artist]
                       [?release :release/name ?rname]]
         edn-coerce [artist-release-pairs]
         constants [100 101 102]
         headers {\"Query\" \"with all options\"}
         to     a-context-key
         params [artist-release-pairs]"
      (lit/map->QueryAction {:name       :with-everything
                             :params     [:artist-release-pairs]
                             :constants  [100 101 102]
                             :query      '[:find  ?release
                                           :in    $ [?aname ?rname]
                                           :where [?artist :artist/name ?aname]
                                           [?release :release/artists ?artist]
                                           [?release :release/name ?rname]]
                             :to         :a-context-key
                             :headers    {"Query" "with all options"}
                             :edn-coerce [:artist-release-pairs]
                             })

      "transact create-user
         properties [user/email user/name user/country]
         operation  vase/assert-entity
         headers    {\"Powered-by\" \"Vase\"}
         to         transaction-result"
      (lit/map->TransactAction {:name :create-user
                                :properties [:user/email :user/name :user/country]
                                :operation  :vase/assert-entity
                                :headers    {"Powered-by" "Vase"}
                                :to         :transaction-result})

      "transact record-input
         properties [an-input]"
      (lit/map->TransactAction {:name :record-input
                                :properties [:an-input]}))

    (testing "specs"
      (are [input expected] (= expected (fern/parse-string input :Spec))
        "spec example.test/age
          (fn [age] (> age 21))"
        {:example.test/age '(fn [age] (> age 21))}))))

(defn block-with-clause [headerline word]
  (str headerline \newline word \space "(fn [& args] true)"))

(defn accepts [start-rule input]
  (not (insta/failure? (fern/parse-string input start-rule))))

(deftest test-http-options
  (testing "basic syntax"
    (are [input expected] (= expected (fern/parse-string input :Http))
      "http"
      {:fern/http {}}

      "http
           allowed-origins (fn [origin] true)
           port 8080
           container-options {:h2c? true}"
      {:fern/http {::http/allowed-origins '(fn [origin] true)
                   ::http/port 8080
                   ::http/container-options '{:h2c? true}}}))

  (testing "all accepted options"
    (are [input] (accepts :Http (block-with-clause "http" input))
      "allowed-origins"
      "container-options"
      "enable-csrf"
      "enable-session"
      "file-path"
      "host"
      "interceptors"
      "method-param-name"
      "mime-types"
      "not-found-interceptor"
      "port"
      "resource-path"
      "router"
      "secure-headers"
      "type")))

(deftest test-incorporate-by-reference
  (testing "apis can use schemas"
    (are [input expected] (= expected (fern/parse-string input :Api))
      "api example/v1
         require example/schema"
      {:vase/apis {:example/v1 {:vase.api/schemas [:example/schema]}}}

      "api example/v2
         require example/base
         require example/extensions"
      {:vase/apis {:example/v2 {:vase.api/schemas [:example/base :example/extensions]}}}

      "api example/v3
         require example/base
         get \"/users\" list-users
         require store/item
         get \"/users/:id\" get-user"
      {:vase/apis {:example/v3 {:vase.api/schemas [:example/base :store/item]
                                :vase.api/routes  {"/users"     {:get ['list-users]}
                                                   "/users/:id" {:get ['get-user]}}}}}))

  (testing "schemas can use schemas"
    (are [input expected] (= expected (map-vals remove-tempids-from-norms (fern/parse-string input :Schema)))
      "schema example/user
         require example/base"
      {:vase/norms
       {:example/user
        {:vase.norm/requires [:example/base]}}}

      "schema example/user
         require example/base
         attribute user/name one string identity \"The user\"
         require persona/enums
         attribute user/persona one ref \"Ref to a persona enum\""
      {:vase/norms
       {:example/user
        {:vase.norm/requires [:example/base :persona/enums]
         :vase.norm/txes [{:db/ident              :user/name
                           :db/valueType          :db.type/string
                           :db/cardinality        :db.cardinality/one
                           :db.install/_attribute :db.part/db
                           :db/doc                "The user"
                           :db/unique             :db.unique/identity}
                          {:db/ident              :user/persona
                           :db/valueType          :db.type/ref
                           :db/cardinality        :db.cardinality/one
                           :db.install/_attribute :db.part/db
                           :db/doc                "Ref to a persona enum"}]}}})))

(deftest test-whitespace-or-comment
  (are [input] (not (insta/failure? (fern/parse-string input)))
    ""
    ";; use pedestal.views"
    "; this is a comment"
    ";\n\n\n\t\t\t\n     ;     "
    ";\r\n\r\n\r\n\t\t\t\r\n   ;   "
    ";; use this \n"
    ";;; with more ;;; inside ;;;"))

(deftest test-comments-blanks-and-trailing-whitespace
  (testing "comments can go anywhere"
    (are [input expected] (= expected (map-vals remove-tempids-from-nested-schema (fern/parse-string input :Api)))
      "; This is an inline comment
       api example/v1
         ; This is an indented comment
         get \"/path\" list-attr1
         ;;; ======================================== ;;;
         ;;; This is a fancy block comment
         ;;; ---------------------------------------- ;;;
         require example/v1.entities"
      {:vase/apis
       {:example/v1
        {:vase.api/routes {"/path" {:get ['list-attr1]}}
         :vase.api/schemas [:example/v1.entities]}}})))

(deftest whole-documents
  (are [input expected] (= expected (fern/parse-string input))
    ""
    [:Description]

    "http
       port 8080"
    [:Description {:fern/http {::http/port 8080}}]

    ";; a sample
     http
       port 8080"
    [:Description {:fern/http {::http/port 8080}}]

    "http port 8080
      type :jetty

    schema example/base"
    [:Description
     {:fern/http
      {::http/port 8080
       ::http/type :jetty}}
     {:vase/norms
      {:example/base
       {}}}]

    "http
     api myapp
       get \"/pets\"  list-pets"
    [:Description
     {:fern/http {}}
     {:vase/apis
      {:myapp
       {:vase.api/routes
        {"/pets" {:get '[list-pets]}}}}}]))

(defmacro compare-parse-failure
  [expected actual]
  `(let [exp# ~expected
         act# ~actual]
     (and
      (= (select-keys act# [:line :column :source-text])
         (select-keys exp# [:line :column :source-text]))
      (not= nil (re-find (:marker-text exp#) (:marker-text act#))))))

(deftest parse-errors-are-reported
  (are [input expected-marker] (compare-parse-failure expected-marker (fern/parse-string input))
    "not-a-top-level-directive 9000"
    {:line   1
     :column 1
     :source-text "not-a-top-level-directive 9000"
     :marker-text #"^Parse error at line 1, column 1.*"}))
