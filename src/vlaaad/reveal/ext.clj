(ns vlaaad.reveal.ext
  (:require [vlaaad.reveal.action :as action]
            [clojure.spec.alpha :as s]
            [clojure.core.specs.alpha :as specs]
            [vlaaad.reveal.stream :as stream]
            [vlaaad.reveal.style :as style]
            [vlaaad.reveal.view :as view]))

;; region streaming

(defmacro defstream
  "Define a streaming for a particular type of value

  `dispatch-val` is a value identifier for streaming, either a class or any
  value that is expected to be in value's `:vlaaad.reveal.steam/type` meta key
  `bindings` is a bindings vector that can have either 1 or 2 args: a streamed
  value and (if needed) an annotation supplied by Reveal streaming process
  `sf` is a streaming function"
  [dispatch-val bindings sf]
  `(stream/defstream ~dispatch-val ~bindings ~sf))

(defn stream
  "Returns streaming function that streams x using default formatting"
  ([x] (stream/stream-dispatch x nil))
  ([x ann] (stream/stream-dispatch x ann)))

(defn as
  "Returns streaming function that streams value using custom provided sf

  When text streamed by `sf` is selected, the value in the context menu will
  be `x`. `view:value` action will be suggested to show the value using default
  streaming"
  [x sf]
  (stream/as x sf))

(defn streamed-as-is
  "Returns streaming function that, when submitted to Reveal UI, is streamed as is"
  [sf]
  (stream/as-is sf))

(defn raw-string
  "Returns streaming function that streams x as a string

  Line breaks etc. are not escaped, will result in new lines"
  ([x] (raw-string x {:fill style/symbol-color}))
  ([x style] (stream/raw-string x style)))

(defn escaped-string
  "Returns streaming function that streams x as string

  `escape` is a function from character to escape string (or nil if there is no
  escape), e.g. {\\newline \"\\\\n\"}"
  ([x escape] (escaped-string x {:fill style/symbol-color} escape {:fill style/scalar-color}))
  ([x style escape] (escaped-string x style escape style))
  ([x style escape escape-style] (stream/escaped-string x style escape escape-style)))

(defn horizontal
  "Returns streaming function that aligns multiple sfs horizontally

  Example:
  11111
  1122222
    22"
  [& sfs]
  (apply stream/horizontal sfs))

(defn vertical
  "Returns streaming function that aligns multiple sfs vertically

  Automatically inserts separators between sfs

  Example:
  111111
  11
  222222
  22"
  [& sfs]
  (apply stream/vertical sfs))

(def separator
  "Streaming function that adds a non-selectable space in horizontal blocks

  Not needed in vertical blocks since they already insert separators"
  stream/separator)

(defn entries
  "Returns streaming function that streams a sequence of entries"
  [m]
  (stream/entries m))

(defn vertically
  "Returns streaming function that streams a sequence of values vertically"
  [xs]
  (stream/vertically xs))

(defn horizontally
  "Returns streaming function that streams a sequence of values horizontally"
  [xs]
  (stream/horizontally xs))

(defn override-style
  "Returns streaming function that changes the style of strings emitted by sf"
  [sf f & args]
  (apply stream/override-style sf f args))

;; endregion

;; region actions

(s/def ::action-id qualified-keyword?)

(s/fdef defaction
  :args (s/cat :action-id ::action-id
               :bindings (s/every ::specs/binding-form :kind vector? :min-count 1 :max-count 2)
               :body (s/+ any?))
  :ret ::action-id)

(defmacro defaction
  "Define action for execution in the context of some selected value

  When user requests a context menu on a selected value, all actions are
  evaluated. If action body returns 0-arg fn, the action is shown in the
  context menu, and the function will be invoked when user selects the action
  for execution. Any other evaluation results, including thrown exceptions, are
  ignored.

  `action-id` is a ns-qualified keyword identifying this action
  `bindings` is a bindings vector that can have either 1 or 2 args: a selected
  value and (if needed) annotation supplied by reveal streaming process
  `body` is an action body that has access to `bindings`, should return 0-arg
  function for action to be available in the context menu"
  [action-id bindings & body]
  `(action/defaction ~action-id ~bindings ~@body))

;; endregion

;; region views

(def value-view
  "Cljfx component fn that shows a value using the streaming system

  Expected keys:
  - `:value` (required) - any value"
  view/value)

(def ref-watch-latest-view
  "Cljfx component fn that shows a continuously updated view of a ref state

  Expected keys:
  - `:ref` (required) - an instance of IRef (e.g. Atom, Var, Ref, Agent)"
  view/ref-watch-latest)

(def ref-watch-all-view
  "Cljfx component fn that shows a log of all ref states

  Expected keys:
  - `:ref` (required) - an instance of IRef (e.g. Atom, Var, Ref, Agent, etc.)"
  view/ref-watch-all)

(def derefable-view
  "Cljfx component fn that asynchronously derefs a value and then shows it

  Expected keys:
  - `:derefable` (required) - a (blocking) derefable (e.g. future, promise)"
  view/derefable)

(def table-view
  "Cljfx component fn that shows a collection of heterogeneous values in a table

  Expected keys:
  - `:items` (required) - a collection of items to show
  - `:columns` (required) - collection of columns that are maps with these keys:
    - `:fn` (required) - a function from item to value for this column
    - `:header` (optional, defaults to `:fn`'s value) - column header"
  view/table)

(defn viewed-as-is
  "Returns a value that, when shown in Results panel, displays a supplied view

  `desc` is a cljfx description that defines a view to show"
  [desc]
  (view/as-is desc))

(defn observable
  "Returns an instance of IRef that wraps another `ref` with `fn` transform"
  [ref fn]
  (view/->Observable ref fn))

(def observable-view
  "Cljfx component fn that shows a custom view produced from values in a ref

  Expected keys:
  - `:ref` (required) - an instance of IRef (e.g. Atom, Var, Ref, Agent etc.)
  - `:fn` (required) - a function from value in a ref to view cljfx description"
  view/observable-view)

(def pie-chart-view
  "Cljfx component fn that shows a pie chart

  Expected keys:
  - `:data` (required) - pie chart data, labeled numbers (e.g. a map from any to
    number, a set of numbers, or a sequence of numbers)"
  view/pie-chart)

(def bar-chart-view
  "Cljfx component fn that shows a bar chart

  Expected keys:
  - `:data` (required) - labeled labeled numbers. Labeled is a collection of
    label to some value, e.g. a map (keys are labels), a set (values are
    themself labels) or a sequential (indices are labels)

  Example:
  ```
  {:fx/type bar-chart-view
   :data {:me {:apples 10 :oranges 5}
          :you {:apples 3 :oranges 15}}}
  ```"
  view/bar-chart)

(def line-chart-view
  "Cljfx component fn that shows a line chart

  Expected keys:
  - `:data` (required) - labeled sequential coll of numbers. Labeled is a
    collection of label to some value, e.g. a map (keys are labels), a set
    (values are themself labels) or a sequential (indices are labels)

  Example:
  ```
  {:fx/type line-chart-view
   :data #{(map #(* % %) (range 100))}}
  ```"
  view/line-chart)

(def scatter-chart-view
  "Cljfx component fn that shows a scatter chart

  Expected keys:
  - `:data` (required) - labeled collection of coordinates (tuples of x and y).
    Labeled is a collection of label to some value, e.g. a map (keys are
    labels), a set (values are themself labels) or a sequential (indices are
    labels)

  Example:
  ```
  {:fx/type scatter-chart-view
   :data {:uniform (repeatedly 500 #(vector (rand) (rand)))
          :gaussian (repeatedly 500 #(vector (* 0.5 (+ (rand) (rand)))
                                             (* 0.5 (+ (rand) (rand)))))}}
  ```"
  view/scatter-chart)

;; endregion