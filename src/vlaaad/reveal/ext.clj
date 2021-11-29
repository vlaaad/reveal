(ns vlaaad.reveal.ext
  (:require [vlaaad.reveal.action :as action]
            [vlaaad.reveal.stream :as stream]
            [vlaaad.reveal.view :as view]
            [vlaaad.reveal.ui :as ui]
            [vlaaad.reveal.action-popup :as action-popup]))

;; region streaming

(defmacro ^{:deprecated "Use vlaaad.reveal ns instead"} defstream [dispatch-val bindings sf]
  `(stream/defstream ~dispatch-val ~bindings ~sf))

(defn ^{:deprecated "Use vlaaad.reveal ns instead"} stream
  ([x] (stream/stream-dispatch x nil))
  ([x ann] (stream/stream-dispatch x ann)))

(defn ^{:deprecated "Use vlaaad.reveal ns instead"} as
  ([x sf] (stream/as x sf))
  ([x ann sf] (stream/as x ann sf)))

(defn ^{:deprecated "Use vlaaad.reveal ns instead"} raw-string
  ([x] (raw-string x {:fill :symbol}))
  ([x style] (stream/raw-string x style)))

(defn ^{:deprecated "Use vlaaad.reveal ns instead"} escaped-string
  ([x escape] (escaped-string x {:fill :symbol} escape {:fill :scalar}))
  ([x style escape] (escaped-string x style escape style))
  ([x style escape escape-style] (stream/escaped-string x style escape escape-style)))

(defn ^{:deprecated "Use vlaaad.reveal ns instead"} horizontal [& sfs]
  (apply stream/horizontal sfs))

(defn ^{:deprecated "Use vlaaad.reveal ns instead"} vertical [& sfs]
  (apply stream/vertical sfs))

(def ^{:deprecated "Use vlaaad.reveal ns instead"} separator stream/separator)

(defn ^{:deprecated "Use vlaaad.reveal ns instead"} entries
  ([m] (stream/entries m))
  ([m ann] (stream/entries m ann)))

(defn ^{:deprecated "Use vlaaad.reveal ns instead"} vertically
  ([xs] (stream/vertically xs))
  ([xs ann] (stream/vertically xs ann)))

(defn ^{:deprecated "Use vlaaad.reveal ns instead"} horizontally
  ([xs] (stream/horizontally xs))
  ([xs ann] (stream/horizontally xs ann)))

(defn ^{:deprecated "Use vlaaad.reveal ns instead"} items
  ([xs] (stream/items xs))
  ([xs ann] (stream/items xs ann)))

(defn ^{:deprecated "Use vlaaad.reveal ns instead"} override-style [sf f & args]
  (apply stream/override-style sf f args))

;; endregion

;; region actions

(defmacro ^{:deprecated "Use vlaaad.reveal ns instead"} defaction [action bindings & body]
  `(action/defaction ~action ~bindings ~@body))

(defn ^{:deprecated "Use vlaaad.reveal ns instead"} execute-action
  ([action value] (execute-action action value nil))
  ([action value annotation] (action/execute action value annotation)))

;; endregion

;; region views

(def ^{:deprecated "Use vlaaad.reveal ns instead"} value-view view/value)

(def ^{:deprecated "Use vlaaad.reveal ns instead"} ref-watch-latest-view view/ref-watch-latest)

(def ^{:deprecated "Use vlaaad.reveal ns instead"} ref-watch-all-view view/ref-watch-all)

(def ^{:deprecated "Use vlaaad.reveal ns instead"} derefable-view view/derefable)

(def ^{:deprecated "Use vlaaad.reveal ns instead"} table-view view/table)

(defn ^{:deprecated "Use vlaaad.reveal ns instead"} observable [ref fn]
  (view/->Observable ref fn))

(def ^{:deprecated "Use vlaaad.reveal ns instead"} observable-view view/observable-view)

(def ^{:deprecated "Use vlaaad.reveal ns instead"} pie-chart-view view/pie-chart)

(def ^{:deprecated "Use vlaaad.reveal ns instead"} bar-chart-view view/bar-chart)

(def ^{:deprecated "Use vlaaad.reveal ns instead"} line-chart-view view/line-chart)

(def ^{:deprecated "Use vlaaad.reveal ns instead"} scatter-chart-view view/scatter-chart)

(def ^{:deprecated "Use vlaaad.reveal ns instead"} popup-view action-popup/ext)

(defn ^{:deprecated "Use vlaaad.reveal ns instead"} action-view [{:keys [action value annotation]}]
  {:fx/type derefable-view
   :derefable (execute-action action value annotation)})

;; endregion

;; region commands

(defn ^{:deprecated "Use vlaaad.reveal ns instead"} command? [x]
  (ui/command? x))

(defn ^{:deprecated "Use vlaaad.reveal ns instead"} submit [value]
  {:vlaaad.reveal/command :vlaaad.reveal.command/event
   :vlaaad.reveal.event/type ::ui/submit
   :value value})

(defn ^{:deprecated "Use vlaaad.reveal ns instead"} clear-output []
  {:vlaaad.reveal/command :vlaaad.reveal.command/event
   :vlaaad.reveal.event/type :vlaaad.reveal.output-panel/on-clear-lines
   :id :output})

(defn ^{:deprecated "Use vlaaad.reveal ns instead"} open-view [value & {:keys [form new-result-panel]}]
  {:vlaaad.reveal/command :vlaaad.reveal.command/event
   :vlaaad.reveal.event/type ::ui/view
   :value value
   :form form
   :target (when new-result-panel :new-result-panel)})

(defn ^{:deprecated "Use vlaaad.reveal ns instead"} close-all-views []
  {:vlaaad.reveal/command :vlaaad.reveal.command/event
   :vlaaad.reveal.event/type ::ui/close-all-views})

(defn ^{:deprecated "Use vlaaad.reveal ns instead"} all [& commands]
  {:vlaaad.reveal/command :vlaaad.reveal.command/event
   :vlaaad.reveal.event/type ::ui/all
   :commands commands})

(defn ^{:deprecated "Use vlaaad.reveal ns instead"} dispose []
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