(ns vlaaad.reveal.ext
  (:require [vlaaad.reveal.action :as action]
            [clojure.spec.alpha :as s]
            [vlaaad.reveal.stream :as stream]
            [vlaaad.reveal.view :as view]
            [vlaaad.reveal.ui :as ui]
            [vlaaad.reveal.action-popup :as action-popup]))

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
  ([x sf] (stream/as x sf))
  ([x ann sf] (stream/as x ann sf)))

(defn raw-string
  "Returns streaming function that streams x as a string

  Line breaks etc. are not escaped, will result in new lines"
  ([x] (raw-string x {:fill :symbol}))
  ([x style] (stream/raw-string x style)))

(defn escaped-string
  "Returns streaming function that streams x as string

  `escape` is a function from character to escape string (or nil if there is no
  escape), e.g. {\\newline \"\\\\n\"}"
  ([x escape] (escaped-string x {:fill :symbol} escape {:fill :scalar}))
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

  Example:
  111111
  11
  222222
  22"
  [& sfs]
  (apply stream/vertical sfs))

(def separator
  "Streaming function that separates values in vertical and horizontal blocks

  In horizontal blocks it's a non-selectable space, in vertical blocks it's
  an empty line"
  stream/separator)

(defn entries
  "Returns streaming function that streams entries (not sfs!)

  Applies the annotation to every streamed key and value"
  ([m] (stream/entries m))
  ([m ann] (stream/entries m ann)))

(defn vertically
  "Returns streaming function that streams values (not sfs!) vertically

  Applies the annotation to every streamed item"
  ([xs] (stream/vertically xs))
  ([xs ann] (stream/vertically xs ann)))

(defn horizontally
  "Returns streaming function that streams values (not sfs!) horizontally

  Applies the annotation to every streamed item"
  ([xs] (stream/horizontally xs))
  ([xs ann] (stream/horizontally xs ann)))

(defn items
  "Returns streaming function that streams values horizontally or vertically

  Might realize the whole `xs` before streaming. Applies the annotation to every
  streamed item"
  ([xs] (stream/items xs))
  ([xs ann] (stream/items xs ann)))

(defn override-style
  "Returns streaming function that changes the style of strings emitted by sf"
  [sf f & args]
  (apply stream/override-style sf f args))

;; endregion

;; region actions

(s/def ::action-id ::action/id)

(s/def defaction `action/defaction)

(defmacro defaction
  "Define action for execution in the context of some selected value

  When user requests a context menu on a selected value, all actions are
  evaluated. If action body returns 0-arg fn, the action is shown in the
  context menu, and the function will be invoked when user selects the action
  for execution. Any other evaluation results, including thrown exceptions, are
  ignored.

  `action` is a ns-qualified keyword identifying this action
  `bindings` is a bindings vector that can have either 1 or 2 args: a selected
  value and (if needed) annotation supplied by Reveal streaming process
  `body` is an action body that has access to `bindings`, should return 0-arg
  function for action to be available in the context menu"
  [action bindings & body]
  `(action/defaction ~action ~bindings ~@body))

(defn execute-action
  "Asynchronously execute registered action on a value

  Returns future with action execution result

  `action` is a ns-qualified keyword identifying action to execute. All
  built-in actions have `vlaaad.reveal.action` ns."
  ([action value]
   (execute-action action value nil))
  ([action value annotation]
   (action/execute action value annotation)))

;; endregion

;; region views

(def value-view
  "Cljfx component fn that shows a value using the streaming system

  Expected keys:
  - `:value` (required) - any value

  Example:
  ```
  {:fx/type value-view
   :value (range 100)}
  ```"
  view/value)

(def ref-watch-latest-view
  "Cljfx component fn that shows a continuously updated view of a ref state

  Expected keys:
  - `:ref` (required) - an instance of IRef (e.g. Atom, Var, Ref, Agent)

  Example:
  ```
  {:fx/type ref-watch-latest-view
   :ref #'inc}
  ```"
  view/ref-watch-latest)

(def ref-watch-all-view
  "Cljfx component fn that shows a log of all ref states

  Expected keys:
  - `:ref` (required) - an instance of IRef (e.g. Atom, Var, Ref, Agent, etc.)

  Example:
  ```
  {:fx/type ref-watch-all-view
   :ref #'inc}
  ```"
  view/ref-watch-all)

(def derefable-view
  "Cljfx component fn that asynchronously derefs a value and then shows it

  Expected keys:
  - `:derefable` (required) - a (blocking) derefable (e.g. future, promise)

  Example:
  ```
  {:fx/type derefable-view
   :derefable (future (Thread/sleep 10000))}
  ```"
  view/derefable)

(def table-view
  "Cljfx component fn that shows a collection of heterogeneous values in a table

  Expected keys:
  - `:items` (required) - a collection of items to show
  - `:columns` (required) - collection of columns that are maps with these keys:
    - `:fn` (required) - a function from item to value for this column
    - `:header` (optional, defaults to `:fn`'s value) - column header

  Example:
  ```
  {:fx/type table-view
   :columns [{:fn identity} {:fn first}]
   :items [[:a 1] {:a 1} \"a=1\"]}
  ```"
  view/table)

(defn observable
  "Returns an instance of IRef that wraps another `ref` with `fn` transform

  Example use:
  ```
  {:fx/type ref-watch-latest-view
   :ref (observable #'foo (juxt identity meta))}
  ```"
  [ref fn]
  (view/->Observable ref fn))

(def observable-view
  "Cljfx component fn that shows a live view produced from some data source

  Expected keys:
  - either (required):
    - `:ref` - an instance of IRef (e.g. Atom, Var, Ref, Agent etc.)
    - `:subscribe` - 1-arg function that performs subscribing, notification and
      unsubscribing for some external data source. It receives a 1-arg function
      used for notifying about new values that may be called immediately. When
      executed, subscribe function should start the notification process. It has
      to return 0-arg function that will be called when the view is disposed,
      so it can unsubscribe from the external data source
  - `:fn` (required) - a function from value in a ref to view cljfx description

  Example:
  ```
  {:fx/type observable-view
   :subscribe (fn [notify]
                (add-tap notify)
                #(remove-tap notify))
   :fn (fn [v] {:fx/type value-view :value v})}
  ```"
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

(def popup-view
  "Cljfx component fn that wraps another component with Reveal popup

  Expected keys:
  - `:desc` (required) - description of a node that will show a popup on context
    menu request
  - either:
    - `:value` (required) and `:annotation` (optional) that will be passed to
      action evaluator for populating the popup
    - `:select` - a function from JavaFX event that triggered a context menu to
      map that represents where and what is selected. Returning nil will not
      trigger a popup. Returned map's expected keys:
      - `:bounds` (required) - JavaFX screen Bounds of a node that will show the
        popup
      - `:value` (required) and `:annotation` (optional) that will be passed to
        action evaluator for populating the popup

  Example:
  ```
  {:fx/type popup-view
   :value (the-ns 'clojure.core)
   :desc {:fx/type :label :text \"The clojure.core library\"}}
  ```"
  action-popup/ext)

(defn action-view
  "Cljfx component fn that shows a view produced by action executed on a value

  Expected keys:
  - `:action` (required) - action id, ns-qualified keyword. All built-in actions
    have `vlaaad.reveal.action` ns
  - `:value` (required) - a value to execute action on
  - `:annotation` (optional) - value annotation expected by action

  Example:
  ```
  {:fx/type action-view
   :action :vlaaad.reveal.action/doc
   :value #'clojure.core/ns}
  ```"
  [{:keys [action value annotation]}]
  {:fx/type derefable-view
   :derefable (execute-action action value annotation)})

;; endregion

;; region commands

(defn command?
  "Check if x is a valid command (i.e. contains `:vlaaad.reveal/command` key)"
  [x]
  (ui/command? x))

(defn submit
  "Returns UI command that submits the `value`"
  [value]
  {:vlaaad.reveal/command :vlaaad.reveal.command/event
   :vlaaad.reveal.event/type ::ui/submit
   :value value})

(defn clear-output
  "Returns UI command that clears the output panel"
  []
  {:vlaaad.reveal/command :vlaaad.reveal.command/event
   :vlaaad.reveal.event/type :vlaaad.reveal.output-panel/on-clear-lines
   :id :output})

(defn open-view
  "Returns UI command that opens the `value` in a result panel

  Optional kv-args:
  - `:form` - objects that is shown in result panel's header
  - `:new-result-panel` - open new result panel even if there already is one"
  [value & {:keys [form new-result-panel]}]
  {:vlaaad.reveal/command :vlaaad.reveal.command/event
   :vlaaad.reveal.event/type ::ui/view
   :value value
   :form form
   :new-result-panel new-result-panel})

(defn close-all-views
  "Returns UI command that closes all open views in every result panel"
  []
  {:vlaaad.reveal/command :vlaaad.reveal.command/event
   :vlaaad.reveal.event/type ::ui/close-all-views})

(defn all
  "Returns UI command that executes a sequence of commands"
  [& commands]
  {:vlaaad.reveal/command :vlaaad.reveal.command/event
   :vlaaad.reveal.event/type ::ui/all
   :commands commands})

(defn dispose
  "Returns UI command that disposes Reveal window"
  []
  {:vlaaad.reveal/command :vlaaad.reveal.command/event
   :vlaaad.reveal.event/type ::ui/quit})

;; endregion

;; region deprecated

(def ^:deprecated view-as-is
  "Returns a value that, when shown in Results panel, displays a supplied view

  `desc` is a cljfx description that defines a view to show

  Deprecated: just return the description"
  identity)

(defn ^:deprecated stream-as-is
  "Returns streaming function that, when streamed as a value, is streamed as is

  Streaming functions are ordinary functions, and functions are streamed by
  default as their class names. This is a way to sidestep streaming value
  formatting and just stream the sf

  Deprecated: all public streaming functions are streamed as is by default"
  [sf]
  (stream/as-is sf))

;; endregion