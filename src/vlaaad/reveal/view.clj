(ns vlaaad.reveal.view
  (:require [vlaaad.reveal.output-panel :as output-panel]
            [vlaaad.reveal.event :as event]
            [vlaaad.reveal.stream :as stream]
            [vlaaad.reveal.action :as action]
            [vlaaad.reveal.style :as style]
            [cljfx.lifecycle :as fx.lifecycle]
            [cljfx.component :as fx.component])
  (:import [clojure.lang IRef]
           [java.util.concurrent ArrayBlockingQueue TimeUnit BlockingQueue]
           [java.util UUID]))

(defn- runduce!
  ([xf x]
   (runduce! xf identity x))
  ([xf f x]
   (let [rf (xf (completing #(f %2)))]
     (rf (rf nil x)))))

(def ext-with-process
  (reify fx.lifecycle/Lifecycle
    (create [_ {:keys [desc start args id] :or {id ::undefined}} opts]
      (let [process-id (if (= ::undefined id) (UUID/randomUUID) id)
            map-event-handler (:fx.opt/map-event-handler opts)
            ret (start process-id args map-event-handler)
            state (-> map-event-handler :*state deref (get process-id))]
        (with-meta {:stop-fn (when (fn? ret) ret)
                    :process-id process-id
                    :fx.opt/map-event-handler map-event-handler
                    :id id
                    :start start
                    :args args
                    :child (fx.lifecycle/create fx.lifecycle/dynamic (into desc state) opts)}
                   {`fx.component/instance #(-> % :child fx.component/instance)})))
    (advance [this component {:keys [desc start args id] :or {id ::undefined} :as this-desc} opts]
      (let [map-event-handler (:fx.opt/map-event-handler opts)]
        (if (and (= start (:start component))
                 (= args (:args component))
                 (= id (:id component))
                 (= map-event-handler (:fx.opt/map-event-handler component)))
          (let [state (-> map-event-handler :*state deref (get (:process-id component)))]
            (update component :child #(fx.lifecycle/advance fx.lifecycle/dynamic % (into desc state) opts)))
          (do
            (fx.lifecycle/delete this component opts)
            (fx.lifecycle/create this this-desc opts)))))
    (delete [_ component opts]
      (when-let [stop-fn (:stop-fn component)]
        (stop-fn))
      (fx.lifecycle/delete fx.lifecycle/dynamic (:child component) opts))))

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
       (.cancel f true)
       (vreset! *running false))))

(defn queue [{:keys [^BlockingQueue queue id]
              :or {id ::undefined}}]
  {:fx/type ext-with-process
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
    #(vreset! *running false)))

(defn value [{:keys [value]}]
  {:fx/type ext-with-process
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
       (.cancel f true))))

(defn ref-watcher [{:keys [ref]}]
  {:fx/type ext-with-process
   :start watch
   :args ref
   :desc {:fx/type output-panel/view}})

(defn as [desc]
  (reify Viewable (make [_] desc)))

(action/register!
  {:id ::watch
   :label "Watch changes"
   :check (fn [v _]
            (when (instance? IRef v)
              #(as {:fx/type ref-watcher :ref v})))})

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
                                                  {:fill ::style/util-color})
                               (stream/stream ({::nil nil} x x)))))))))]
    (submit! @*ref)
    (add-watch *ref watch-key #(submit! %4))
    #(do
       (remove-watch *ref watch-key)
       (vreset! *running false)
       (.cancel f true))))

(defn ref-logger [{:keys [ref]}]
  {:fx/type ext-with-process
   :start log
   :args ref
   :desc {:fx/type output-panel/view}})

(action/register!
  {:id ::log
   :label "Log changes"
   :check (fn [v _]
            (when (instance? IRef v)
              #(as {:fx/type ref-logger :ref v})))})

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
    #(.cancel f true)))

(defn- blocking-deref-view [{:keys [state] :as props}]
  (case state
    ::waiting {:fx/type :label
               :focus-traversable true
               :text "Loading..."}
    ::value (make (:value props))
    ::exception (make (:exception props))))

(defn blocking-deref [{:keys [blocking-deref]}]
  {:fx/type ext-with-process
   :start deref-process
   :args blocking-deref
   :desc {:fx/type blocking-deref-view}})

(extend-protocol Viewable
  nil
  (make [this] {:fx/type value :value this})

  Object
  (make [this] {:fx/type value :value this}))
