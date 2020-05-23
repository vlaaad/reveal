(ns vlaaad.reveal.view
  (:require [vlaaad.reveal.output-panel :as output-panel]
            [vlaaad.reveal.popup :as popup]
            [vlaaad.reveal.event :as event]
            [vlaaad.reveal.stream :as stream]
            [vlaaad.reveal.action :as action]
            [vlaaad.reveal.style :as style]
            [vlaaad.reveal.fx :as rfx]
            [cljfx.api :as fx]
            [cljfx.prop :as fx.prop]
            [cljfx.lifecycle :as fx.lifecycle]
            [cljfx.mutator :as fx.mutator]
            [cljfx.fx.table-cell :as fx.table-cell])
  (:import [clojure.lang IRef]
           [java.util.concurrent ArrayBlockingQueue TimeUnit BlockingQueue]
           [javafx.scene.control TableView TableColumn TablePosition]
           [javafx.scene Node]
           [javafx.css PseudoClass]))

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

(defn- select-bounds-and-value! [^TableView view]
  (when-let [^TablePosition pos (first (.getSelectedCells (.getSelectionModel view)))]
    (when-let [cell (->> (.lookupAll view ".reveal-table-cell:selected")
                         (some #(when (contains? (.getPseudoClassStates ^Node %)
                                                 (PseudoClass/getPseudoClass "selected"))
                                  %)))]
      {:bounds (.localToScreen cell (.getBoundsInLocal cell))
       :value (.getCellData (.getTableColumn pos) (.getRow pos))})))

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
                       [{:header 'value :cell identity}])
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