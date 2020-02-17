(ns vlaaad.reveal.view
  (:require [vlaaad.reveal.output-panel :as output-panel]
            [vlaaad.reveal.event :as event]
            [vlaaad.reveal.stream :as stream]
            [vlaaad.reveal.layout :as layout]))

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
     (let [t (Thread.
               ^Runnable
               (fn []
                 (try
                   (runduce! (comp stream/stream-xf (partition-all 128))
                             #(if (.isInterrupted (Thread/currentThread))
                                (reduced nil)
                                (dispatch! {::event/handler output-panel/on-add-lines
                                            :fx/event %
                                            :id (:id env)}))
                             value)
                   (catch InterruptedException _)))
               "reveal-output-panel-value-thread")]
       (.start t)
       #(.interrupt t))))
  {:fx/type output-panel/view
   :id (:id env)
   :layout (layout/make)})

(extend-protocol Viewable
  nil
  (get-value [this] this)
  (make-view [this env] (value this env))

  Object
  (get-value [this] this)
  (make-view [this env] (value this env)))
