(ns vlaaad.reveal.view
  (:require [vlaaad.reveal.output-panel :as output-panel]
            [vlaaad.reveal.event :as event]
            [vlaaad.reveal.stream :as stream]
            [vlaaad.reveal.layout :as layout]
            [vlaaad.reveal.action :as action]
            [vlaaad.reveal.style :as style])
  (:import [clojure.lang IRef]
           [java.util.concurrent ArrayBlockingQueue TimeUnit]))

(defprotocol Viewable
  (get-value [this] "Returns value of that viewable")
  (make-view [this env] "Returns cljfx description for the viewable"))

(defn- runduce!
  ([xf x]
   (runduce! xf identity x))
  ([xf f x]
   (let [rf (xf (completing #(f %2)))]
     (rf (rf nil x)))))

(defn value [value env]
  ((:on-create env)
   (fn [dispatch!]
     (let [*running (volatile! true)]
       (.submit event/daemon-executor
                ^Runnable
                (fn []
                  (runduce! (comp stream/stream-xf
                                  (partition-all 128)
                                  (take-while (fn [_] @*running)))
                            #(dispatch! {::event/handler output-panel/on-add-lines
                                         :fx/event %
                                         :id (:id env)})
                            value)))
       #(vreset! *running false))))
  {:fx/type output-panel/view
   :id (:id env)
   :layout (layout/make)})

(defn ref-watcher [*ref]
  (reify Viewable
    (get-value [_] *ref)
    (make-view [_ env]
      ((:on-create env)
       (fn [dispatch!]
         (let [*running (volatile! true)
               ;; todo don't need queue here, just a single value
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
                                (dispatch! {::event/handler output-panel/on-clear-lines
                                            :id (:id env)})
                                (runduce! (comp stream/stream-xf
                                                (partition-all 128)
                                                (take-while
                                                  (fn [_] (and @*running
                                                               (nil? (.peek out-queue))))))
                                          #(dispatch! {::event/handler output-panel/on-add-lines
                                                       :fx/event %
                                                       :id (:id env)})
                                          ({::nil nil} x x))))))]
           (submit! @*ref)
           (add-watch *ref watch-key #(submit! %4))
           #(do
              (remove-watch *ref watch-key)
              (vreset! *running false)
              (.cancel f true)))))
      {:fx/type output-panel/view
       :id (:id env)
       :layout (layout/make)})))

(action/register!
  {:id ::watch
   :label "Watch changes"
   :check (fn [v _]
            (when (instance? IRef v)
              #(ref-watcher v)))})

(defn ref-logger [*ref]
  (reify Viewable
    (get-value [_] *ref)
    (make-view [_ env]
      ((:on-create env)
       (fn [dispatch!]
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
                                  #(dispatch! {::event/handler output-panel/on-add-lines
                                               :fx/event %
                                               :id (:id env)})
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
              (.cancel f true)))))
      {:fx/type output-panel/view
       :id (:id env)
       :layout (layout/make)})))

(action/register!
  {:id ::log
   :label "Log changes"
   :check (fn [v _]
            (when (instance? IRef v)
              #(ref-logger v)))})

(extend-protocol Viewable
  nil
  (get-value [this] this)
  (make-view [this env] (value this env))

  Object
  (get-value [this] this)
  (make-view [this env] (value this env)))
