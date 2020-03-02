(ns vlaaad.reveal.view
  (:require [vlaaad.reveal.output-panel :as output-panel]
            [vlaaad.reveal.event :as event]
            [vlaaad.reveal.stream :as stream]
            [vlaaad.reveal.layout :as layout]
            [vlaaad.reveal.action :as action])
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
     (let [*running (volatile! true)
           t (Thread.
               ^Runnable
               (fn []
                 (runduce! (comp stream/stream-xf
                                 (partition-all 128)
                                 (take-while (fn [_] @*running)))
                           #(dispatch! {::event/handler output-panel/on-add-lines
                                        :fx/event %
                                        :id (:id env)})
                           value))
               "reveal-view-value-thread")]
       (.start t)
       #(vreset! *running false))))
  {:fx/type output-panel/view
   :id (:id env)
   :layout (layout/make)})

(defn watcher [*ref]
  (reify Viewable
    (get-value [_] *ref)
    (make-view [_ env]
      ((:on-create env)
       (fn [dispatch!]
         (let [*running (volatile! true)
               out-queue (ArrayBlockingQueue. 1024)
               submit! #(.put out-queue (if (nil? %) ::stream-nil %))
               watch-key (gensym "vlaaad.reveal.view/watcher")
               t (Thread.
                   ^Runnable
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
                                   (if (= ::stream-nil x) nil x)))))
                   "reveal-view-watcher-thread")]
           (.start t)
           (submit! @*ref)
           (add-watch *ref watch-key #(submit! %4))
           #(do
              (remove-watch *ref watch-key)
              (vreset! *running false)))))
      {:fx/type output-panel/view
       :id (:id env)
       :layout (layout/make)})))

(action/register!
  {:id ::watch
   :label "Watch"
   :check (fn [vals+anns]
            (when-let [v (first (peek vals+anns))]
              (when (instance? IRef v)
                #(watcher v))))})

(extend-protocol Viewable
  nil
  (get-value [this] this)
  (make-view [this env] (value this env))

  Object
  (get-value [this] this)
  (make-view [this env] (value this env)))
