(ns vlaaad.reveal.view
  (:require [vlaaad.reveal.output-panel :as output-panel]
            [vlaaad.reveal.popup :as popup]
            [vlaaad.reveal.event :as event]
            [vlaaad.reveal.stream :as stream]
            [vlaaad.reveal.action :as action]
            [vlaaad.reveal.style :as style]
            vlaaad.reveal.doc
            [vlaaad.reveal.fx :as rfx]
            [cljfx.api :as fx]
            [cljfx.prop :as fx.prop]
            [cljfx.mutator :as fx.mutator]
            [cljfx.lifecycle :as fx.lifecycle])
  (:import [clojure.lang IRef]
           [java.util.concurrent ArrayBlockingQueue TimeUnit BlockingQueue]
           [javafx.scene.control TableView TablePosition]
           [javafx.scene Node]
           [javafx.css PseudoClass]
           [java.net URL URI]
           [javafx.event Event]))

(defn- runduce!
  ([xf x]
   (runduce! xf identity x))
  ([xf f x]
   (let [rf (xf (completing #(f %2)))]
     (rf (rf nil x)))))

(defmethod event/handle ::dispose-state [*state {:keys [id]}]
  (swap! *state dissoc id))

(defmethod event/handle ::create-view-state [*state {:keys [id state]}]
  (swap! *state assoc id (assoc state :id id)))

(defn- process-queue [id ^BlockingQueue queue handler]
  (handler {::event/type ::create-view-state :id id :state (output-panel/make)})
  (let [*running (volatile! true)
        add-lines! #(handler {::event/type ::output-panel/on-add-lines :id id :fx/event %})
        xform (comp stream/stream-xf
                    (partition-all 128)
                    (take-while (fn [_] @*running)))
        f (.submit event/daemon-executor
                   ^Runnable
                   (fn []
                     (while @*running
                       (let [x (.take queue)]
                         (runduce! xform add-lines! ({::nil nil} x x))))))]
    #(do
       (handler {::event/type ::dispose-state :id id})
       (.cancel f true)
       (vreset! *running false))))

(defn queue [{:keys [^BlockingQueue queue id]
              :or {id ::rfx/undefined}}]
  {:fx/type rfx/ext-with-process
   :id id
   :start process-queue
   :args queue
   :desc {:fx/type output-panel/view}})

(defprotocol Viewable
  (make [this] "Returns cljfx description for the viewable"))

(defn- process-value [id value handler]
  (handler {::event/type ::create-view-state :id id :state (output-panel/make)})
  (let [*running (volatile! true)
        add-lines! #(handler {::event/type ::output-panel/on-add-lines :id id :fx/event %})
        xform (comp stream/stream-xf
                    (partition-all 128)
                    (take-while (fn [_] @*running)))]
    (.submit event/daemon-executor ^Runnable (fn [] (runduce! xform add-lines! value)))
    #(do
       (vreset! *running false)
       (handler {::event/type ::dispose-state :id id}))))

(defn value [{:keys [value]}]
  {:fx/type rfx/ext-with-process
   :start process-value
   :args value
   :desc {:fx/type output-panel/view}})

(defn- watch [id *ref handler]
  (handler {::event/type ::create-view-state :id id :state (output-panel/make)})
  (let [*running (volatile! true)
        out-queue (ArrayBlockingQueue. 1024)
        submit! #(.put out-queue ({nil ::nil} % %))
        watch-key (gensym "vlaaad.reveal.view/watcher")
        f (.submit event/daemon-executor ^Runnable
                   (fn []
                     (while @*running
                       (when-some [x (loop [x (.poll out-queue 1 TimeUnit/SECONDS)
                                            found nil]
                                       (if (some? x)
                                         (recur (.poll out-queue) x)
                                         found))]
                         (handler {::event/type ::output-panel/on-clear-lines :id id})
                         (runduce! (comp stream/stream-xf
                                         (partition-all 128)
                                         (take-while
                                           (fn [_] (and @*running
                                                        (nil? (.peek out-queue))))))
                                   #(handler {::event/type ::output-panel/on-add-lines
                                              :fx/event %
                                              :id id})
                                   ({::nil nil} x x))))))]
    (submit! @*ref)
    (add-watch *ref watch-key #(submit! %4))
    #(do
       (remove-watch *ref watch-key)
       (vreset! *running false)
       (.cancel f true)
       (handler {::event/type ::dispose-state :id id}))))

(defn ref-watcher [{:keys [ref]}]
  {:fx/type rfx/ext-with-process
   :start watch
   :args ref
   :desc {:fx/type output-panel/view}})

(defn as [desc]
  (reify Viewable (make [_] desc)))

(action/def ::watch:latest [v]
  (when (instance? IRef v)
    #(as {:fx/type ref-watcher :ref v})))

(defn- log [id *ref handler]
  (handler {::event/type ::create-view-state :id id :state (output-panel/make)})
  (let [*running (volatile! true)
        out-queue (ArrayBlockingQueue. 1024)
        submit! #(.put out-queue ({nil ::nil} % %))
        watch-key (gensym "vlaaad.reveal.view/watcher")
        *counter (volatile! -1)
        f (.submit event/daemon-executor ^Runnable
                   (fn []
                     (while @*running
                       (let [x (.take out-queue)]
                         (runduce!
                           (comp stream/stream-xf
                                 (partition-all 128)
                                 (take-while
                                   (fn [_] @*running)))
                           #(handler {::event/type ::output-panel/on-add-lines
                                      :fx/event %
                                      :id id})
                           (stream/just
                             (stream/horizontal
                               (stream/raw-string (format "%4d: " (vswap! *counter inc))
                                                  {:fill style/util-color})
                               (stream/stream ({::nil nil} x x)))))))))]
    (submit! @*ref)
    (add-watch *ref watch-key #(submit! %4))
    #(do
       (remove-watch *ref watch-key)
       (vreset! *running false)
       (.cancel f true)
       (handler {::event/type ::dispose-state :id id}))))

(defn ref-logger [{:keys [ref]}]
  {:fx/type rfx/ext-with-process
   :start log
   :args ref
   :desc {:fx/type output-panel/view}})

(action/def ::watch:all [v]
  (when (instance? IRef v)
    #(as {:fx/type ref-logger :ref v})))

(defn- deref-process [id blocking-deref handler]
  (handler {::event/type ::create-view-state :id id :state {:state ::waiting}})
  (let [f (.submit event/daemon-executor ^Runnable
                   (fn []
                     (try
                       (handler {::event/type ::create-view-state
                                 :id id
                                 :state {:state ::value :value @blocking-deref}})
                       (catch Throwable e
                         (handler {::event/type ::create-view-state
                                   :id id
                                   :state {:state ::exception :exception e}})))))]
    #(do
       (.cancel f true)
       (handler {::event/type ::dispose-state :id id}))))

(defn- blocking-deref-view [{:keys [state] :as props}]
  (case state
    ::waiting {:fx/type :label
               :focus-traversable true
               :text "Loading..."}
    ::value (make (:value props))
    ::exception (make (:exception props))))

(defn blocking-deref [{:keys [blocking-deref]}]
  {:fx/type rfx/ext-with-process
   :start deref-process
   :args blocking-deref
   :desc {:fx/type blocking-deref-view}})

(extend-protocol Viewable
  nil
  (make [this] {:fx/type value :value this})

  Object
  (make [this] {:fx/type value :value this}))

(defn summary [{:keys [value max-length]
                :or {max-length 48}}]
  {:fx/type :group
   :children [(stream/fx-summary max-length value)]})

(defn- describe-cell [x]
  {:content-display :graphic-only
   :style-class "reveal-table-cell"
   :graphic {:fx/type summary :value x}})

(defn- initialize-table! [^TableView view]
  (.selectFirst (.getSelectionModel view))
  (.setCellSelectionEnabled (.getSelectionModel view) true))

(defn- select-bounds-and-value! [^Event event]
  (let [^TableView view (.getSource event)]
    (when-let [^TablePosition pos (first (.getSelectedCells (.getSelectionModel view)))]
      (when-let [cell (->> (.lookupAll view ".reveal-table-cell:selected")
                           (some #(when (contains? (.getPseudoClassStates ^Node %)
                                                   (PseudoClass/getPseudoClass "selected"))
                                    %)))]
        {:bounds (.localToScreen cell (.getBoundsInLocal cell))
         :value (.getCellData (.getTableColumn pos) (.getRow pos))}))))

;; todo table view does not like repeated items, should use indices!

(defn table [{:keys [seqable]}]
  (let [xs (seq seqable)
        head (first xs)
        columns (->> (cond
                       (map? head)
                       (for [k (keys head)]
                         {:header k :cell #(get % k)})

                       (map-entry? head)
                       [{:header 'key :cell key} {:header 'val :cell val}]

                       (indexed? head)
                       (for [i (range (count head))]
                         {:header i :cell #(nth % i)})

                       :else
                       [{:header 'item :cell identity}])
                     (map (fn [{:keys [cell header]}]
                            {:fx/type :table-column
                             :style-class "reveal-table-column"
                             :min-width 40
                             :graphic {:fx/type summary
                                       :value header}
                             :cell-factory {:fx/cell-type :table-cell
                                            :describe describe-cell}
                             :cell-value-factory (fn [x]
                                                   (try
                                                     (cell x)
                                                     (catch Throwable e
                                                       e)))})))]
    {:fx/type fx/ext-on-instance-lifecycle
     :on-created initialize-table!
     :desc {:fx/type popup/ext
            :select select-bounds-and-value!
            :desc {:fx/type :table-view
                   :style-class "reveal-table"
                   :columns columns
                   :items (vec xs)}}}))

(action/def ::view:table [v]
  (when (and (some? v)
             (not (string? v))
             (seqable? v))
    #(as {:fx/type table :seqable v})))

(vlaaad.reveal.action/def ::open-web-page [v]
  (when (or (instance? URI v)
            (instance? URL v)
            (and (string? v) (re-matches #"^https?://.+" v)))
    #(as {:fx/type :web-view
          :url (str v)})))

(defn- request-source-focus! [^Event e]
  (.requestFocus ^Node (.getSource e)))

(defn- tagged->values [tagged]
  (cond-> tagged (map? tagged) vals))

(defn- tagged->tag+values [tagged]
  (if (map? tagged)
    tagged
    (map-indexed vector tagged)))

(defn- tagged?
  "Check if every value in a coll of specified size has uniquely identifying tag"
  [x pred & {:keys [min max]
             :or {min 1 max 32}}]
  (and (or (map? x)
           (sequential? x))
       (<= min (bounded-count (inc max) x) max)
       (every? pred (tagged->values x))))

(defn pie-chart [{:keys [data]}]
  {:fx/type :pie-chart
   :style-class "reveal-chart"
   :on-mouse-pressed request-source-focus!
   :animated false
   :data (for [[k v] (tagged->tag+values data)]
           {:fx/type :pie-chart-data
            :name (stream/str-summary k)
            :pie-value v})})

(action/def ::view:pie-chart [x]
  (when (tagged? x number? :min 2)
    #(as {:fx/type pie-chart :data x})))

(def ^:private ext-with-value-on-node
  (fx/make-ext-with-props
    {::value (fx.prop/make
               (fx.mutator/setter (fn [^Node node value]
                                    (if (some? value)
                                      (.put (.getProperties node) ::value value)
                                      (.remove (.getProperties node) ::value))))
               fx.lifecycle/scalar)}))

(defn- select-chart-node! [^Event event]
  (let [^Node node (.getTarget event)]
    (when-let [value (::value (.getProperties node))]
      {:value value
       :bounds (.localToScreen node (.getBoundsInLocal node))})))

(defn- numbered? [x]
  (or (number? x)
      (and (sequential? x)
           (number? (first x)))))

(defn- numbered->number [numbered]
  (cond-> numbered (not (number? numbered)) first))

(defn bar-chart [{:keys [data]}]
  {:fx/type popup/ext
   :select select-chart-node!
   :desc {:fx/type :bar-chart
          :style-class "reveal-chart"
          :on-mouse-pressed request-source-focus!
          :animated false
          :x-axis {:fx/type :category-axis :label "key"}
          :y-axis {:fx/type :number-axis :label "value"}
          :data (for [[series v] (tagged->tag+values data)]
                  {:fx/type :xy-chart-series
                   :name (stream/str-summary series)
                   :data (for [[key value] (tagged->tag+values v)]
                           {:fx/type :xy-chart-data
                            :x-value (stream/->str key)
                            :y-value (numbered->number value)
                            :node {:fx/type ext-with-value-on-node
                                   :props {::value {:value value
                                                    :key key
                                                    :series series}}
                                   :desc {:fx/type :region}}})})}})

(action/def ::view:bar-chart [x]
  (when-let [data (cond
                    (tagged? x numbered?)
                    {x x}

                    (tagged? x #(tagged? % numbered?))
                    x)]
    #(as {:fx/type bar-chart
          :data data})))

(defn- numbereds? [x]
  (and (sequential? x)
       (<= 2 (bounded-count 1025 x) 1024)
       (every? numbered? x)))

(defn line-chart [{:keys [data]}]
  {:fx/type popup/ext
   :select select-chart-node!
   :desc {:fx/type :line-chart
          :style-class "reveal-chart"
          :on-mouse-pressed request-source-focus!
          :animated false
          :x-axis {:fx/type :number-axis
                   :label "index"
                   :auto-ranging false
                   :lower-bound 0
                   :upper-bound (dec (transduce
                                       (comp (map second) (map count))
                                       max
                                       0
                                       (tagged->tag+values data)))
                   :tick-unit 10
                   :minor-tick-count 10}
          :y-axis {:fx/type :number-axis :label "value"}
          :data (for [[series numbers] (tagged->tag+values data)]
                  {:fx/type :xy-chart-series
                   :name (stream/str-summary series)
                   :data (->> numbers
                              (map-indexed
                                (fn [index value]
                                  {:fx/type :xy-chart-data
                                   :x-value index
                                   :y-value (numbered->number value)
                                   :node {:fx/type ext-with-value-on-node
                                          :props {::value {:value value
                                                           :index index
                                                           :series series}}
                                          :desc {:fx/type :region}}})))})}})

(action/def ::view:line-chart [x]
  (when-let [data (cond
                    (numbereds? x)
                    {x x}

                    (tagged? x numbereds?)
                    x)]
    #(as {:fx/type line-chart :data data})))

(defn- coordinate? [x]
  (and (sequential? x)
       (= 2 (bounded-count 3 x))
       (number? (nth x 0))
       (number? (nth x 1))))

(defn- scattered? [x]
  (or (coordinate? x)
      (and (sequential? x)
           (coordinate? (first x)))))

(defn- scattered->coordinate [x]
  (let [f (first x)]
    (if (sequential? f) f x)))

(defn- scattereds? [x]
  (and (coll? x)
       (<= 1 (bounded-count 1025 x) 1024)
       (every? scattered? x)))

(defn scatter-chart [{:keys [data]}]
  {:fx/type popup/ext
   :select select-chart-node!
   :desc {:fx/type :scatter-chart
          :style-class "reveal-chart"
          :on-mouse-pressed request-source-focus!
          :animated false
          :x-axis {:fx/type :number-axis :label "x" :force-zero-in-range false}
          :y-axis {:fx/type :number-axis :label "y" :force-zero-in-range false}
          :data (for [[series places] (tagged->tag+values data)]
                  {:fx/type :xy-chart-series
                   :name (stream/str-summary series)
                   :data (for [value places
                               :let [[x y :as с] (scattered->coordinate value)]]
                           {:fx/type :xy-chart-data
                            :x-value x
                            :y-value y
                            :node {:fx/type ext-with-value-on-node
                                   :props {::value {:value value
                                                    :coordinate с
                                                    :series series}}
                                   :desc {:fx/type :region}}})})}})

(action/def ::view:scatter-chart [x]
  (when-let [data (cond
                    (tagged? x scattereds?)
                    x

                    (scattereds? x)
                    {x x})]
    #(as {:fx/type scatter-chart :data data})))
