<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Fern](#fern)
  - [Why do we need Fern?](#why-do-we-need-fern)
  - [Is the EDN format going away?](#is-the-edn-format-going-away)
  - [Introducing the language](#introducing-the-language)
  - [Escape Hatch](#escape-hatch)
  - [Quick Re-fern-erance](#quick-re-fern-erance)
    - [http](#http)
    - [schema](#schema)
      - [Attribute definition](#attribute-definition)
    - [api](#api)
      - [Route definition](#route-definition)
    - [Query - Return data](#query---return-data)
    - [Transact - Create datoms](#transact---create-datoms)
    - [Conform - Reshape context data](#conform---reshape-context-data)
    - [Respond - Build an immediate response](#respond---build-an-immediate-response)
    - [Redirect - Respond with a new location](#redirect---respond-with-a-new-location)
  - [Terminal vs. non-terminal interceptors](#terminal-vs-non-terminal-interceptors)
  - [Use Cases](#use-cases)
    - [Running from vase.jar](#running-from-vasejar)
    - [Working with a Service Map](#working-with-a-service-map)
    - [Working with a Vase descriptor](#working-with-a-vase-descriptor)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Fern

Fern is an input language (external DSL) for Vase. It fills the same
role as EDN files for Vase descriptors.

## Why do we need Fern?

EDN can be difficult to work with for humans. It becomes very deeply
nested, so it requires mental overhead to keep track of which part of
the specification is under the cursor. EDN is also unforgiving. It is
pretty common for a small error in a Vase descriptor to prevent the
whole thing from working. In some cases, an error in the descriptor
prevents you from even starting a REPL.

Fern is an external DSL. There is a parser for it and a grammar
specification. (See below.) By its very nature, we can produce more
informative error messages.

## Is the EDN format going away?

No, not at all. The EDN format maps directly into the input data
structure that Vase really uses. Fern provides a layer of syntactic
sugar. The output of Fern is a Vase descriptor just the same as if you
used `vase/load-edn-file`.

## Introducing the language

Fern uses very simple syntax. It is more like a data format than a
coding language. With Fern you declare the parts of your service and
their relationships. Here is a small snippet:

```
api example/exchange-rate
  get "/rates" list-rates
  get "/rates/:currency-pair" lookup-rate
  put "/rates/:currency-pair" [authenticated? authorized? post-rate]
```

Let's break that down a bit. The first line declares a Vase API. An
API is a set of related routes and (optionally) schema. This API is
named "example/exchange-rate". Internally, Fern translates that into
the Clojure keyword `:example/exchange-rate` that Vase expects.

The next three lines declare routes within the API. Each route
contains a verb, a path, and either one interceptor or a vector of
interceptors. The two `get` routes each use a single interceptor,
while the `put` route adds some additional interceptors to prevent
unauthorized modifications.

Those interceptor names correspond to the Vase interceptors that you
would put into each route in the EDN file. Instead of nesting the
`#vase/query`, `#vase/transact`, etc. inside the routes, Fern flattens
those out and has you refer to them by name. So Fern expects to find
interceptors named `list-rates`, `lookup-rate`, `authenticated?`,
`authorized?`, and `post-rate` _somewhere_ in the file. Fern does
_not_ enforce definition order the way Clojure does. You can structure
your file top-down, bottom-up, or whatever order suits your style.

The interceptors are defined right in the same file, like this:

```
query list-rates
  q [:find ?ccy1 ?ccy2 ?rate
     :where [?xing :exchange/ccy1 ?ccy1]
            [?xing :exchange/ccy2 ?ccy2]
            [?xing :exchange/rate ?rate]]
```

The first line in this block says we're using a query interceptor. The
`query` block accepts some options that closely align with the keys
[described here](action_literals.md). In this case, we're just using
the "q" option that specifies the query to run. Using "q" is meant to
resemble the
[Datomic API call](http://docs.datomic.com/clojure/index.html#datomic.api/q)
for running a query.

Notice that the query is an ordinary Clojure expression, embedded in
the Fern file. This expression is a datalog query, exactly as
described in the
[Datomic documentation](http://docs.datomic.com/query.html). Fern
allows you to supply Clojure expressions as the body of many options.

"Query" is just one of the basic Vase
interceptors. The whole set are:

   * query
   * transact
   * respond
   * redirect
   * conform

([Validate](action_literals.md#vasevalidate) is
still on probation, so we haven't added it to Fern.)

## Escape Hatch

Then there's the "universal interceptor" block. You can create any
arbitrary interceptor by specifying it's enter, leave, and error
functions directly in the Fern file like this:

```
interceptor com.example/track-invocation
  enter (fn [ctx] (assoc ctx :enter-was-called true))
  leave (fn [ctx] (assoc ctx :leave-was-called true))
  error (fn [ctx] (assoc ctx :error-was-called true))
```

Fern doesn't allow you to create new top-level functions or embed
large chunks of Clojure code. It's really intended as a declaration
language for APIs. So if you need more than a simple function like
these, it's best to create it in a separate `.clj` file, make sure to
require that namespace from your main namespace, and _then_ invoke
your complex functions from the Fern file.

## Quick Re-fern-erance

The authoritative source on which options each interceptor takes is
the source code itself. The grammar in fern.clj defines exactly what
each one can do. But, not everybody enjoys reading EBNF grammars, so
here's a quick reference.

### http

Defines: An HTTP listener and it's settings.

Uses: One or more APIs. If no APIs are specified in the "http" block
then it implicitly uses all APIs.

Sample:

```
http
  port 80
  api use com.example/users
```

Options: All options are as described in the
[service map](http://pedestal.io/reference/service-map) reference. Any
of them can be literals or expressions that evaluate to the expected values.

| Option name             |
|-------------------------|
| `allowed-origins`       |
| `container-options`     |
| `enable-csrf`           |
| `enable-session`        |
| `file-path`             |
| `host`                  |
| `interceptors`          |
| `method-param-name`     |
| `mime-types`            |
| `not-found-interceptor` |
| `port`                  |
| `resource-path`         |
| `router`                |
| `secure-headers`        |
| `type`                  |

### schema

Defines: Attributes in a "norm" that Vase will transact into your database

Uses: One or more schemas.

Sample:

```
schema foo/bar
  schema use foo/other
  attribute bar-length one long "Length of the bar, in cubits"
```

Options:

| Option name             | Meaning |
|-------------------------|---------|
| `attribute`       | An attribute definition as described below |
| `schema use`      | Reference to the name of another schema that should be transacted before this one |

#### Attribute definition

attribute \_ident\_ \_cardinality\_ \_value-type\_ \_options\_ \_docstring\_

* Ident - any Clojure symbol. Do not put the colon (':') in front of
  this like you would with a keyword.
* Cardinality - `one` or `many`
* Value type - `long`, `double`, `instant`, `ref`, `bigint`, `float`,
  `string`, `keyword`, `bigdec`, `bytes`, `uri`, `uuid`, or `boolean`
* Options - Any sequence of `unique`, `identity`, `index`, `fulltext`,
  `component`, or `no-history`
* Docstring - a string enclosed with double-quotes


### api

Defines: A single API, its required schema, its routes, and any
interceptors that need to be on every route.

Uses: One or more schemas

Sample

```
api gamify/v1
  schema use my-cool-schema
  interceptors [valid-user? valid-game?]
  get "/highscore/:user/:game" achievements
  put "/highscore/:user/:game" [detect-cheats post-score]
```

Options:

| Option name | Meaning |
|-------------|---------|
| `schema use` | Reference to the name of a schema that should be transacted before using this API |
| `interceptors` | An interceptor name or vector of interceptor names that shold be prepended to every route's specific interceptors |
| `get`, `put,` `post`, `options`, `delete`, `head` | Define a route as described below |

#### Route definition

verb \_path\_ \_interceptor-or-vector\_

Each route has a path as described in Pedestal's
[routing quick reference](http://pedestal.io/reference/routing-quick-reference). Requests
that match the path will be handled via the interceptors named in
\_interceptor-or-vector\_.

### Query - Return data

Sample

```
query list-rates
  q [:find ?ccy1 ?ccy2 ?rate
     :where [?xing :exchange/ccy1 ?ccy1]
            [?xing :exchange/ccy2 ?ccy2]
            [?xing :exchange/rate ?rate]]
```

Options:

| Option name | Meaning |
|-------------|---------|
| `q` | Datalog query to execute |
| `params` | Vector of param names (without colons) to supply to the query as arguments |
| `constants` | Vector of constant values to supply to the query as arguments. These follow the params. |
| `headers` | Map of additional header values for the HTTP response |
| `to` | Name of a key on the context that should receive the query data. Defaults are as specified for the [literals](action_literals.md). |

### Transact - Create datoms

Sample
```
transact com.example/add-puppies
  operation assert-entity
  properties [puppy/breed puppy/name]
```

Options:

| Option name | Meaning |
|-------------|---------|
| `operation` | Either `assert-entity` or `retract-entity`. If omitted, `assert-entity` is assumed. |
| `properties` | Vector of property names (without colons) to be used from a request's body-params. |
| `headers` | Map of additional header values for the HTTP response |
| `to` | Name of a key on the context that should receive the transaction result data. Defaults are as specified for the [literals](action_literals.md). |

### Conform - Reshape context data

Sample

```
conform
  from a-context-key
  with-spec (s/and string? #(.startsWith % "test-"))
  to other-key
```

Options:

| Option name | Meaning |
|-------------|---------|
| `from` | The name of a key (no colon). The data to conform will be located on the interceptor (context map)[http://pedestal.io/reference/context-map] at that key. |
| `to` | The name of a key (no colon). The conformed data will be attached to the context map at this key. Both `to` and `from` can be the same key. |
| `spec` | A Clojure expression that evaluates to a spec. (That includes spec names.) |

### Respond - Build an immediate response

Sample

```
respond
  status 200
  body "This is a body"
```

Options:

| Option name | Meaning |
|-------------|---------|
| `status`  | Expression that evaluates to an HTTP status code |
| `params`  | Vector of param names (without colons) that will be available to the body. |
| `edn-coerce` | Vector of param names (without colons) that will be read by clojure.edn/read-string before use. |
| `headers` | Map of additional header values for the HTTP response |
| `body` | A Clojure expression that will be evaluated with the params named in `params` (possibly converted as in `edn-coerce`) bound to their values from the (request map)[http://pedestal.io/reference/request-map]. The return value becomes the HTTP response body. |

### Redirect - Respond with a new location

```
respond
  url "www.google.com"
```

Options:

| Option name | Meaning |
|-------------|---------|
| `status`  | Expression that evaluates to an HTTP status code. If omitted, 302 is assumed |
| `params`  | Vector of param names (without colons) that will be available to the body. |
| `headers` | Map of additional header values for the HTTP response |
| `body` | A Clojure expression that will be evaluated with the params named in `params` (possibly converted as in `edn-coerce`) bound to their values from the (request map)[http://pedestal.io/reference/request-map]. The return value becomes the HTTP response body. |
| `url` | A Clojure expression whose value will be placed in the "Location" header. |

## Terminal vs. non-terminal interceptors

Some interceptors make sense only in the terminal (i.e., final) position in a chain. A few interceptors _may_ be in termainal position but may also be in non-terminal position:

| Interceptor | Terminal? |
|-------------|-----------|
| Respond     | Always    |
| Redirect    | Always    |
| Query       | Allowed   |
| Transact    | Allowed   |
| Conform     | Never     |
| Intercept   | Depends on how you use it.  |

## Use Cases

Fern goes in a text file. Depending on how you call the parser, you
either get back a full Pedestal service map, already started, or you
get a Vase descriptor.

### Running from vase.jar

If all you need are stock interceptors, then you don't even have to
create a new project to use Vase and Pedestal. You can just run the
"vase.jar" file with `java -jar` like this:

    java -jar vase.jar _my-api.fern_

This runs the same code as shown in the next section.

### Working with a Service Map

Fern can produce the whole service map if you call
`vase.fern/load-service`. That service is ready to start with
Pedestal.

```clojure
(ns example.service
  (:require
    [com.cognitect.vase.fern :as fern]
    [io.pedestal.http :as http]))

(defn wait-for-stop
  "Blocks until Jetty exits"
  [service]
  (.join (::http/server service))

(defn start
  [fern-path]
  (-> fern-path
      fern/load-service
      http/start
      http/join))
```

With this format, you can specify all the HTTP goodies for your
[service map](http://pedestal.io/reference/service-map) right in the
Fern file. It uses a block at the top of the file that looks like
this:

```clojure
http
  port 80
  resource-path "public"
  allowed-origins (constantly true)
```

If you don't have any such block, then you get Pedestal's defaults for
everything in the service map.

Just those two files are enough to start a server. It's not going to
do anything but serve static files from the classpath, but it's a
start.

### Working with a Vase descriptor

The function `com.cognitect.vase.fern/load-descriptor` loads a Fern
file and returns it. In the event of any problems, Fern returns a map
with the key `:markers` bound to a vector of marker maps. Each map
has:

   * `:line` - The line number where the problem occurred.
   * `:column` - The column number where the problem occurred.
   * `:source-text` - The original source text related to this problem.
   * `:marker-text` - A human-readable description of the problem.
   * `:references` - (Sometimes) A list of marker maps that are
     related to this problem.

Most of the time, you'll want to log any problems that arise in
processing the file.

Here is an example of using a descriptor within your own service map:

```
(defn service
  [fern-path]
  (let [descriptor (fern/load-descriptor fern-path)]
    {::http/port   80
     ::http/routes (conj app-routes (vase/routes descriptor))
     ::http/join?  (not dev-mode)
     ::http/type   :jetty}))
```

Notice that `vase/routes` is called as usual.
