(ns vlaaad.reveal
  (:require [clojure.edn :as edn]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [vlaaad.reveal.action :as action]
            vlaaad.reveal.memory
            [vlaaad.reveal.action-popup :as action-popup]
            [vlaaad.reveal.graphviz :as graphviz]
            [vlaaad.reveal.stream :as stream]
            [vlaaad.reveal.test :as test]
            [vlaaad.reveal.view :as view]
            [vlaaad.reveal.vega :as vega]
            [vlaaad.reveal.ui :as ui])
  (:import [javafx.application Platform]))

;; region repl

(defn- update-some [m k f & args]
  (let [v (get m k ::not-found)]
    (if (= v ::not-found)
      m
      (assoc m k (apply f v args)))))

(defn- resolve-fn [x]
  (if (symbol? x)
    @(requiring-resolve x)
    x))

(defn- hydrate-fns [m & ks]
  (reduce #(update-some %1 %2 resolve-fn) m ks))

(defn ^:cli repl
  "Start reveal repl that wraps clojure.main/repl

  Evaluating :repl/quit will quit the repl

  Available opts (all optional):

    ;; reveal-specific repl opts
    :infer-ns    attempt to infer expected ns for evaluation from the supplied
                 form metadata, default false

    ;; repl opts as per clojure.main/repl doc (can be fully-qualified symbols)
    :init           0-arg fn, initialization hook
    :need-prompt    0-arg fn, will prompt user for input if returns true
    :prompt         0-arg fn that prompts user for input
    :flush          0-arg fn that flushes output
    :read           2-arg fn that either reads, asks for prompt or quits repl
    :eval           1-arg fn that evaluates its argument
    :print          1-arg fn that prints its argument
    :caught         1-arg fn that handles thrown exceptions

    ;; window opts:
    :title              window title, defaults to \"repl\"
    :close-difficulty   how easy it is to close the window; either:
                        * :easy - close on Escape
                        * :normal - close on OS close window shortcut
                        * :hard (default) - close on shortcut + confirmation
    :always-on-top      whether the window is always on top of other windows,
                        defaults to false
    :decorations        whether to show OS window decorations, defaults to
                        inverse of :always-on-top
    :bounds             any value indicating window bounds group, defaults to
                        'vlaaad.reveal.ui/repl. Every group remembers window
                        bounds - location and size - and reuses them between
                        shown windows

  Examples:

    ;; start the repl with pretty-print console output
    (repl :print clojure.pprint/pprint)

    ;; start the repl as a sticker
    (repl :always-on-top true)"
  ([] (repl {}))
  ([k v & kvs] (repl (apply hash-map k v kvs)))
  ([opts]
   ((requiring-resolve 'vlaaad.reveal.repl/repl)
    (hydrate-fns opts :init :need-prompt :prompt :flush :read :eval :print :caught))))

(defn ^:cli io-prepl
  "Start reveal prepl bound to *in* and *out*

  Evaluating :repl/quit will quit the prepl

  Available opts (all optional):

    ;; io-prepl opts
    :valf   1-arg fn that serializes returned and tapped values, default pr-str
    ;; window opts
    :title              window title, defaults to \"io-prepl\"
    :close-difficulty   how easy it is to close the window; either:
                        * :easy - close on Escape
                        * :normal - close on OS close window shortcut
                        * :hard (default) - close on shortcut + confirmation
    :always-on-top      whether the window is always on top of other windows,
                        defaults to false
    :decorations        whether to show OS window decorations, defaults to
                        inverse of :always-on-top
    :bounds             any value indicating window bounds group, defaults to
                        'vlaaad.reveal.ui/repl. Every group remembers window
                        bounds - location and size - and reuses them between
                        shown windows"
  ([] (io-prepl {}))
  ([k v & kvs] (io-prepl (apply hash-map k v kvs)))
  ([opts]
   ((requiring-resolve 'vlaaad.reveal.prepl/io-prepl) (hydrate-fns opts :valf))))

(defn ^:cli remote-prepl
  "Start a client reveal prepl that connects to a remote prepl

  Reveal is more useful when it runs in the target process, having full access
  to the objects in the VM. With remote prepl, it can only access data received
  from the network. It is useful for e.g. connecting to remote JVMs that don't
  include Reveal on the classpath or to ClojureScript prepls.

  Required opts:

    :port        target port

  Optional opts:

    ;; remote prepl opts (fns can be fully-qualified symbols pointing to vars)
    :host        target host
    :in-reader   LineNumberingPushbackReader to read forms from, defaults to *in*
    :out-fn      1-arg fn that consumes prepl output, defaults to prn
    :valf        1-arg fn that deserializes returned and tapped values, defaults
                 to edn string reader with tagged-literal support
    :readf       2-arg fn of in-reader and EOF value that reads from in-reader,
                 defaults to #(read %1 false %2)
    ;; window opts
    :title              window title, defaults to \"remote-repl on [host:]port\"
    :close-difficulty   how easy it is to close the window; either:
                        * :easy - close on Escape
                        * :normal - close on OS close window shortcut
                        * :hard (default) - close on shortcut + confirmation
    :always-on-top      whether the window is always on top of other windows,
                        defaults to false
    :decorations        whether to show OS window decorations, defaults to
                        inverse of :always-on-top
    :bounds             any value indicating window bounds group, defaults to
                        'vlaaad.reveal.ui/repl. Every group remembers window
                        bounds - location and size - and reuses them between
                        shown windows

  Examples:

    ;; connect to localhost
    (remote-prepl :port 5555)

    ;; connect over the network
    (remote-prepl :host \"192.168.1.15\" :port 5555)

    ;; without deserialization
    (remote-prepl :port 5555 :valf identity)

    ;; custom title
    (remote-prepl :port 5555 :title \"cljs-dev\")"
  ([] (remote-prepl {}))
  ([k v & kvs] (remote-prepl (apply hash-map k v kvs)))
  ([opts]
   ((requiring-resolve 'vlaaad.reveal.prepl/remote-prepl)
    (hydrate-fns opts :valf :readf :in-reader :out-fn))))

;; endregion

;; region ui

(defn ui
  "Create and show Reveal UI window

  Returns multi-arity function:
  - 0-arg call will dispose the window
  - 1-arg call will submit supplied value to main output panel, unless the value
    contains :vlaaad.reveal/command key - in that case the window will execute
    the command; returns its argument

  Reveal command is either a value produced from command function in this ns or
  a map with these keys:

    :vlaaad.reveal/command   code form that will be evaluated by Reveal
    :env                     optional map from symbols to arbitrary values that
                             will be let-bound in evaluated command form
    :ns                      optional ns used for evaluation, defaults to
                             'vlaaad.reveal

  Available opts (all optional):

    :title              window title
    :close-difficulty   how easy it is to close the window; either:
                        * :easy - close on Escape
                        * :normal (default) - close on OS close window shortcut
                        * :hard - close on shortcut + confirmation
    :always-on-top      whether the window is always on top of other windows,
                        defaults to false
    :decorations        whether to show OS window decorations, defaults to
                        inverse of :always-on-top
    :bounds             any value indicating window bounds group, defaults to
                        :default. Every group remembers window bounds - location
                        and size - and reuses them between shown windows"
  ([] (ui {}))
  ([k v & kvs] (ui (apply hash-map k v kvs)))
  ([args]
   (ui/make-queue (update args :default #(or % :normal)))))

(defn tap-log
  "Open a new sticker window that logs all tapped values

  Available opts (all optional):

    :title              window title, defaults to \"tap log\"
    :close-difficulty   how easy it is to close the window; either:
                        * :easy - close on Escape
                        * :normal (default) - close on OS close window shortcut
                        * :hard - close on shortcut + confirmation
    :always-on-top      whether the window is always on top of other windows,
                        defaults to true
    :decorations        whether to show OS window decorations, defaults to
                        inverse of :always-on-top
    :bounds             any value indicating window bounds group, defaults to
                        'vlaaad.reveal.ui/tap-log. Every group remembers window
                        bounds - location and size - and reuses them between
                        shown windows"
  [& {:as opts}]
  (ui/tap-log opts))

(defn inspect
  "Open new window to inspect x

  Returns x

  Available opts (all optional):

    :title              window title, defaults to \"inspect\"
    :close-difficulty   how easy it is to close the window; either:
                        * :easy (default) - close on Escape
                        * :normal - close on OS close window shortcut
                        * :hard - close on shortcut + confirmation
    :always-on-top      whether the window is always on top of other windows,
                        defaults to true
    :decorations        whether to show OS window decorations, defaults to
                        inverse of :always-on-top
    :bounds             any value indicating window bounds group, defaults to
                        'vlaaad.reveal.ui/inspect. Every group remembers window
                        bounds - location and size - and reuses them between
                        shown windows"
  [x & {:as opts}]
  (ui/inspect x opts))

(defn sticker
  "Open a new sticker window that shows x

  Returns nil

  Available opts (all optional):

    :title              window title, defaults to \"inspect\"
    :close-difficulty   how easy it is to close the window; either:
                        * :easy - close on Escape
                        * :normal (default) - close on OS close window shortcut
                        * :hard - close on shortcut + confirmation
    :always-on-top      whether the window is always on top of other windows,
                        defaults to true
    :decorations        whether to show OS window decorations, defaults to
                        inverse of :always-on-top
    :bounds             any value indicating window bounds group, defaults to
                        :title value if provided. Every group remembers window
                        bounds - location and size - and reuses them between
                        shown windows"
  [x & {:as opts}]
  (ui/sticker x opts))

;; endregion

;; region streaming

(defmacro defstream
  "Define a streaming for a particular type of value

  Args:

    dispatch-val   value identifier for streaming, either a class or any value
                   that is expected to be in value's :vlaaad.reveal.steam/type
                   meta key
    bindings       vector that can have either 1 or 2 args: a streamed value and
                   (optional) an annotation supplied by Reveal streaming process
    sf             is a streaming function"
  [dispatch-val bindings sf]
  `(stream/defstream ~dispatch-val ~bindings ~sf))

(defn stream
  "Returns streaming function that streams x using default formatting"
  ([x] (stream/stream-dispatch x nil))
  ([x ann] (stream/stream-dispatch x ann)))

(defn as
  "Returns streaming function that streams value using custom provided sf

  When text streamed by sf is selected, the value in the context menu will
  be x. view:value action will be suggested to show the value using its default
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

  Args:

    x              any object that will be coerced to string
    style          unescaped text styling map
    escape         1-arg fn from character to escape string or nil if there is
                   no escape, e.g. {\\newline \"\\\\n\"}
    escape-style   escaped text styling map"
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

  Might realize the whole xs before streaming. Applies the annotation to every
  streamed item"
  ([xs] (stream/items xs))
  ([xs ann] (stream/items xs ann)))

(defn override-style
  "Returns streaming function that changes the style of strings emitted by sf"
  [sf f & args]
  (apply stream/override-style sf f args))

;; endregion

;; region actions

(s/def defaction `action/defaction)

(defmacro defaction
  "Define action for execution in the context of some selected value

  When user requests a context menu on a selected value, all actions are
  evaluated. If action body returns 0-arg fn, the action is shown in the
  context menu, and the function will be invoked when user selects the action
  for execution. Any other evaluation results, including thrown exceptions, are
  ignored.

  Args:

    action     ns-qualified keyword identifying this action
    bindings   vector that can have either 1 or 2 args: a selected value and
               (if needed) annotation supplied by Reveal streaming process
    body       is an action body that has access to bindings, should return
               0-arg fn for action to be available in the context menu"
  [action bindings & body]
  `(action/defaction ~action ~bindings ~@body))

(defn execute-action
  "Asynchronously execute registered action on a value

  Returns future with action execution result

  Args:

    action       ns-qualified keyword identifying action to execute. All
                 built-in actions use vlaaad.reveal.action ns
    value        any object to execute the action on
    annotation   optional annotation that can be used by actions"
  ([action value]
   (execute-action action value nil))
  ([action value annotation]
   (action/execute action value annotation)))

;; endregion

;; region test

(defn test-runner
  "Create reveal test runner object

  A test can be either:
  * Namespace instance or simple symbol that points to namespace
  * Var instance or qualified symbol that points to Var
  * :everything to test all Vars in directories on the classpath
  * a map with optional :include and :exclude keys whose vals are collections of
    idents that define what tests from directories on the classpath to include
    or exclude, where:
    - a simple symbol describes Namespace to include/exclude
    - a qualified symbol describes Var to include/exclude
    - a keyword describes Var or Namespace metadata key to include/exclude
  * a collection of tests"
  ([]
   (test-runner :everything))
  ([test]
   (test/runner test)))

(defn run-tests!
  "Run the tests defined by test runner if they are not already running"
  [runner]
  (test/test! runner))

(defn test-runner-view
  "Cljfx component that shows test runner controls, summary and output

  Optional keys:

    :runner    test runner created with [[test-runner]]

  Example:

    {:fx/type test-runner-view
     :runner (test-runner)}"
  [{:keys [runner]}]
  {:fx/type test/runner-view
   :runner runner})

(defn test-runner-controls-view
  "Cljfx component that shows only test runner controls and summary

  Clicking on a summary will open the test output

  Optional keys:

    :runner    test runner created with [[test-runner]]

  Example:

    {:fx/type test-runner-controls-view
     :runner (test-runner)}"
  [{:keys [runner]}]
  {:fx/type test/controls-view
   :runner runner
   :open-view-button true})

;; easy, controls+output
(def ^{:arglists '([{:keys [test auto-run]
                     :or {test :everything
                          auto-run false}}])}
  test-view
  "Cljfx component that shows test UI with controls, summary and output

  This is a convenience component that automatically creates an instance of
  test runner and optionally starts it

  Optional keys:

    :test        the test to run, defaults to :everything. Valid values are:
                 * Namespace instance or simple symbol that points to namespace
                 * Var instance or qualified symbol that points to Var
                 * :everything to test all Vars in directories on the classpath
                 * a map with optional :include and :exclude keys whose vals are
                   collections of idents that define what tests from directories
                   on the classpath to include or exclude, where:
                   - a simple symbol describes Namespace to include/exclude
                   - a qualified symbol describes Var to include/exclude
                   - a keyword describes Var or Namespace metadata key
                     to include/exclude
                 * a collection of tests
    :auto-run    a flag indicating whether to start the test run when the view
                 is shown or not, defaults to false

  Example:

    {:fx/type test-view
     :test '{:exclude [:generative my.ns.with.long.tests]}}"
  (fn [props]
    (assoc props :fx/type test/test-view)))

;; easy, controls+button to open output
(def
  ^{:arglists '([& {:keys [test auto-run]
                    :or {test :everything
                         auto-run false}}])}
  test-sticker
  "Opens a new sticker window with test runner controls and summary

  Test output can be opened by clicking on a summary.

  Optional kv-args:

    ;; test opts
    :test        the test to run, defaults to :everything. Valid values are:
                 * Namespace instance or simple symbol that points to namespace
                 * Var instance or qualified symbol that points to Var
                 * :everything to test all Vars in directories on the classpath
                 * a map with optional :include and :exclude keys whose vals are
                   collections of idents that define what tests from directories
                   on the classpath to include or exclude, where:
                   - a simple symbol describes Namespace to include/exclude
                   - a qualified symbol describes Var to include/exclude
                   - a keyword describes Var or Namespace metadata key
                     to include/exclude
                 * a collection of tests
    :auto-run    a flag indicating whether to start the test run when the view
                 is shown or not, defaults to false
    ;; window opts
    :title              window title, defaults to \"test the-test\"
    :close-difficulty   how easy it is to close the window; either:
                        * :easy - close on Escape
                        * :normal (default) - close on OS close window shortcut
                        * :hard - close on shortcut + confirmation
    :always-on-top      whether the window is always on top of other windows,
                        defaults to true
    :decorations        whether to show OS window decorations, defaults to
                        inverse of :always-on-top
    :bounds             any value indicating window bounds group, defaults to
                        :title value if provided. Every group remembers window
                        bounds - location and size - and reuses them between
                        shown windows"
  (fn [& {:as opts}]
    (test/sticker opts)))

;; endregion

;; region views

(def value-view
  "Cljfx component fn that shows a value using the streaming system

  Required keys:

    :value   any value

  Example:

    {:fx/type value-view
     :value (all-ns)}"
  view/value)

(def ref-watch-latest-view
  "Cljfx component fn that shows a continuously updated view of a ref state

  Required keys:

    :ref    an instance of IRef (e.g. Atom, Var, Ref, Agent)

  Example:

    {:fx/type ref-watch-latest-view
     :ref #'my.app/integrant-system}"
  view/ref-watch-latest)

(def ref-watch-all-view
  "Cljfx component fn that shows a log of all ref states

  Required keys:

    :ref    an instance of IRef (e.g. Atom, Var, Ref, Agent, etc.)

  Example:

    {:fx/type ref-watch-all-view
     :ref #'inc}"
  view/ref-watch-all)

(def derefable-view
  "Cljfx component fn that asynchronously derefs a value and then shows it

  Expected keys:

    :derefable    a (blocking) derefable (e.g. future, promise)

  Example:

    {:fx/type derefable-view
     :derefable (future (Thread/sleep 10000))}"
  view/derefable)

(def table-view
  "Cljfx component fn that shows a collection of homogeneous values in a table

  Required keys:

    :items     collection of items to show
    :columns   collection of columns that are maps with these keys:
               * :fn (required) - fn from item to value for this column
               * :header (optional, defaults to `:fn`'s value) - column header
               * :columns (optional) - nested columns

  Example:
    {:fx/type table-view
     :columns [{:fn identity} {:fn first}]
     :items [[:a 1] {:a 1} \"a=1\"]}"
  view/table)

(def ^{:arglists '([{:keys [branch? root children render annotate valuate]
                     :or {render identity
                          valuate identity
                          annotate (constantly nil)}}])}
  tree-view
  "Cljfx component fn that shows a tree view

  Required keys:

    :root        a root node of the tree
    :branch?     predicate that tests if a node is a branch
    :children    1-arg fn that converts tree node that is known to be a branch
                 to a coll of children nodes.

  Optional keys:

    :render      1-arg fn that converts the node to value that will be displayed
                 in a tree cell (e.g. to a cljfx description or an object to
                 display using the streaming printing system)
    :valuate     1-arg fn that converts node to value that can be further
                 explored with action/popup system
    :annotate    1-arg fn that converts node to optional annotation map that can
                 be used by actions

  Example:

    {:fx/type tree-view
     :root [0 [1 [2] [[3]]] [4 [5 [[6 7 8]]]]]
     :branch? seqable?
     :children seq}"
  (fn [props]
    (assoc props :fx/type view/tree-view)))

(defn observable
  "Returns an instance of IRef that wraps another ref with fn transform

  Example:

    {:fx/type ref-watch-latest-view
     :ref (observable #'foo (juxt identity meta))}"
  [ref fn]
  (view/->Observable ref fn))

(def observable-view
  "Cljfx component fn that shows a live view produced from some data source

  Expected keys:

    :ref          an instance of IRef (e.g. Atom, Var, Ref, Agent etc.)
    :subscribe    1-arg fn that performs subscribing, notification and
                  unsubscribing for some external data source. It receives 1-arg
                  fn used for notifying about new values that may be called
                  immediately. When executed, subscribe function should start
                  the notification process. It has to return 0-arg function that
                  will be called when the view is disposed, so it can
                  unsubscribe from the external data source
    :fn           1-arg fn from value in a ref to view cljfx description

  Either :ref or :subscribe is required, as well as :fn

  Example:

    {:fx/type observable-view
     :subscribe (fn [notify]
                  (add-tap notify)
                  #(remove-tap notify))
     :fn (fn [v] {:fx/type value-view :value v})}"
  view/observable-view)

(def pie-chart-view
  "Cljfx component fn that shows a pie chart

  Required keys:

    :data    pie chart data, labeled numbers (e.g. a map from any to number,
             a set of numbers, or a sequence of numbers)"
  view/pie-chart)

(def bar-chart-view
  "Cljfx component fn that shows a bar chart

  Required keys:

    :data    labeled labeled numbers. Labeled is a collection of label to some
             value, e.g. a map (keys are labels), a set (values are themselves
             labels) or a sequential (indices are labels)

  Example:

    {:fx/type bar-chart-view
     :data {:me {:apples 10 :oranges 5}
            :you {:apples 3 :oranges 15}}}"
  view/bar-chart)

(def line-chart-view
  "Cljfx component fn that shows a line chart

  Required keys:

    :data    labeled sequential coll of numbers. Labeled is a collection of
             label to some value, e.g. a map (keys are labels), a set (values
             are themselves labels) or a sequential (indices are labels)

  Example:

    {:fx/type line-chart-view
     :data #{(map #(* % %) (range 100))}}"
  view/line-chart)

(def scatter-chart-view
  "Cljfx component fn that shows a scatter chart

  Required keys:

    :data    labeled collection of coordinates (tuples of x and y). Labeled is
             a collection of label to some value, e.g. a map (keys are labels),
             a set (values are themselves labels) or a sequential (indices are
             labels)

  Example:

    {:fx/type scatter-chart-view
     :data {:uniform (repeatedly 500 #(vector (rand) (rand)))
            :gaussian (repeatedly 500 #(vector (* 0.5 (+ (rand) (rand)))
                                               (* 0.5 (+ (rand) (rand)))))}}"
  view/scatter-chart)

(def popup-view
  "Cljfx component fn that wraps another component with Reveal popup

  Accepted keys:

    :desc            cljfx description of a node that will show a popup on
                     context menu request
    either:
    * :value         a value to execute action on
      :annotation    optional annotation map that might be used by actions
    * :select        1-arg fn from JavaFX event that triggered a context menu to
                     map that represents where and what is selected. Returning
                     nil will not trigger a popup. Returned map's expected keys:
                     * :bounds (required) - JavaFX screen Bounds of a node that
                     will show the popup
                     * :value (required) - a value to execute action on
                     * :annotation (optional) - annotation map that might be
                       used by actions

  Either :value or :select is required, as well as :desc

  Example:

    {:fx/type popup-view
     :value (the-ns 'clojure.core)
     :desc {:fx/type :label :text \"The clojure.core library\"}}"
  action-popup/ext)

(defn action-view
  "Cljfx component fn that shows a view produced by action executed on a value

  Required keys:

    :action        an action id, ns-qualified keyword. All built-in actions use
                   vlaaad.reveal.action ns
    :value         a value to execute action on

  Optional keys:

    :annotation    value annotation map that might be used by action

  Example:

    {:fx/type action-view
     :action :vlaaad.reveal.action/doc
     :value #'clojure.core/ns}"
  [{:keys [action value annotation]}]
  {:fx/type derefable-view
   :derefable (execute-action action value annotation)})

(defn ^{:arglists '([{:keys [spec opt data signals on-signals]}])} vega-view
  "Cljfx component fn that shows a vega(-lite) visualization

  Required keys:

    :spec          vega(-lite) spec, either URL string that points to spec, or
                   map that can be serialized to vega(-lite) json spec

  Optional keys:

    :opt           map that can be serialized to json vega-embed opt:
                   https://github.com/vega/vega-embed#options
    :data          either:
                   * vega dataset - a coll of map datums that will be assigned
                     to \"source\" dataset (which is a default name if vega spec
                     does not specify a named dataset)
                   * a map from dataset names to dataset colls
                   using :data instead of inlining dataset coll inside a spec
                   has much better update performance in data streaming use case
    :signals       map from signal name to signal value (will be serialized
                   to json)
    :on-signals    map from signal name to 1-arg fn that will be called with new
                   signal values (will be deserialized from json with
                   keyword keys)

  Examples:

    ;; simple barchart

    {:fx/type vega-view
     :spec {:mark :bar
            :encoding {:x {:field :a
                           :type :nominal
                           :axis {:labelAngle 0}}
                       :y {:field :b
                           :type :quantitative}}}
     :data [{:a \"A\" :b 28}
            {:a \"B\" :b 55}
            {:a \"C\" :b 43}
            {:a \"D\" :b 91}
            {:a \"E\" :b 81}
            {:a \"F\" :b 53}
            {:a \"G\" :b 19}
            {:a \"H\" :b 87}
            {:a \"I\" :b 52}]}

    ;; REPL-driven signals - redefine signals var to control UI inputs

    (def signals
      {\"CylYr_Cylinders\" 6
       \"CylYr_Year\" 1978})

    {:fx/type observable-view
     :ref #'signals
     :fn (fn [signals]
           {:fx/type vega-view
            :signals signals
            :spec
            {:data {:url \"https://vega.github.io/vega-lite/data/cars.json\"}
             :transform [{:calculate \"year(datum.Year)\" :as \"Year\"}]
             :layer
             [{:params
               [{:name \"CylYr\"
                 :value [{:Cylinders 4 :Year 1977}]
                 :select {:type \"point\" :fields [\"Cylinders\" \"Year\"]}
                 :bind {:Cylinders {:input \"range\" :min 3 :max 8 :step 1}
                        :Year {:input \"range\" :min 1969 :max 1981 :step 1}}}]
               :mark :circle
               :encoding {:x {:field \"Horsepower\" :type :quantitative}
                          :y {:field \"Miles_per_Gallon\" :type :quantitative}
                          :color {:condition {:param \"CylYr\"
                                              :field \"Origin\"
                                              :type :nominal}
                                  :value :grey}}}
              {:transform [{:filter {:param \"CylYr\"}}]
               :mark :circle
               :encoding {:x {:field \"Horsepower\" :type :quantitative}
                          :y {:field \"Miles_per_Gallon\" :type :quantitative}
                          :color {:field \"Origin\" :type :nominal}
                          :size {:value 100}}}]}})}"
  [props]
  (assoc props :fx/type vega/view))

(def ^{:arglists '([{:keys [graph]}])} graphviz-view
  "Cljfx component fn that shows a graphviz visualization

  Required keys:
    :graph    graphviz graph string

  Examples:
    {:fx/type graphviz-view :graph \"digraph { a -> b; b -> c }\"}"
  graphviz/view)

;; endregion

;; region commands

(defn command?
  "Check if x is a valid UI command (i.e. contains :vlaaad.reveal/command key)"
  [x]
  (ui/command? x))

(defn submit-command!
  "Submit command to open reveal windows that match a predicate

  Returns nil

  Args:

    pred       predicate of window options map, e.g. any? or :always-on-top;
               defaults to any?
    command    UI command"
  ([command]
   (ui/submit! any? command))
  ([pred command]
   (ui/submit! pred command)))

(defn submit
  "Returns UI command that submits the value to queue based Reveal window

  Queue-based windows are Reveal REPLs and windows created with ui fn"
  [value]
  {:vlaaad.reveal/command :vlaaad.reveal.command/event
   :vlaaad.reveal.event/type ::ui/submit
   :value value})

(defn clear-output
  "Returns UI command that clears the output panel

  This command only works on Reveal windows that use streaming output panel as
  a root view"
  []
  {:vlaaad.reveal/command :vlaaad.reveal.command/event
   :vlaaad.reveal.event/type :vlaaad.reveal.output-panel/on-clear-lines
   :id :output})

(defn open-view
  "Returns UI command that shows the value in a separate view

  Optional kv-args:

    :form      objects that is shown in result panel's header
    :target    where to show the view, by default it uses the nearest result
               panel, can be changed with either:
               * :inspector - show in a new inspector sticker window
               * :new-result-panel - show in a new result panel"
  [value & {:keys [form target]}]
  {:vlaaad.reveal/command :vlaaad.reveal.command/event
   :vlaaad.reveal.event/type ::ui/view
   :value value
   :form form
   :target target})

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
  "Returns UI command that disposes the window"
  []
  {:vlaaad.reveal/command :vlaaad.reveal.command/event
   :vlaaad.reveal.event/type ::ui/quit})

(defn minimize
  "Returns UI command that minimizes the window"
  []
  {:vlaaad.reveal/command :vlaaad.reveal.command/event
   :vlaaad.reveal.event/type ::ui/minimize})

(defn restore
  "Returns UI command that restores previously minimized window"
  []
  {:vlaaad.reveal/command :vlaaad.reveal.command/event
   :vlaaad.reveal.event/type ::ui/restore})

(defn toggle-minimized
  "Returns UI command that toggles minimized/restored state of the window"
  []
  {:vlaaad.reveal/command :vlaaad.reveal.command/event
   :vlaaad.reveal.event/type ::ui/toggle-minimized})

;; endregion

;; region main

(defn- ^:cli help
  "Display help for a specific command"
  [command]
  (let [m (meta ((ns-interns 'vlaaad.reveal) command))
        doc (or (and (:cli m) (:doc m))
                (throw (Exception. (str "Unknown command: " command))))
        lines (str/split-lines doc)
        indented-lines (->> lines
                            next
                            (remove str/blank?)
                            (map #(count (take-while #{\space} %))))
        indent (if (seq indented-lines) (apply min indented-lines) 0)
        re-indent (re-pattern (str "^\\s{" indent "}"))]
    (->> lines
         (map #(str/replace % re-indent ""))
         (run! println))))

(defn -main [& [command & args]]
  (if (contains? #{nil "-?" "-h" "--help"} command)
    (do (println "Command line entry point for launching reveal repls\n\nAvailable commands:")
        (let [sym+vars (->> (ns-interns 'vlaaad.reveal)
                            (filter #(-> % val meta :cli))
                            (sort-by #(-> % val meta :line)))
              pad (apply max (map #(-> % key name count) sym+vars))]
          (doseq [[sym var] sym+vars]
            (println (format (str "  %-" pad "s  %s") sym (first (str/split-lines (:doc (meta var))))))))
        (println "\nUse 'clj ... -M -m vlaaad.reveal help <command>' to see full description of that command"))
    (apply (or (let [var ((ns-interns 'vlaaad.reveal) (symbol command))]
                 (and (:cli (meta var)) var))
               (throw (Exception. (str "Unknown command: " command))))
           (map edn/read-string args)))
  (Platform/exit))

;; endregion