(ns vlaaad.reveal.event
  (:import [java.util.concurrent Executors ThreadFactory ExecutorService]
           [clojure.lang IFn]))

(def ^ExecutorService daemon-executor
  (let [*counter (atom 0)
        factory (reify ThreadFactory
                  (newThread [_ runnable]
                    (doto (Thread. runnable (str "reveal-agent-pool-" (swap! *counter inc)))
                      (.setDaemon true))))]
    (Executors/newCachedThreadPool factory)))

(defn daemon-future-call [f]
  (.submit daemon-executor ^Callable (bound-fn* f)))

(defmacro daemon-future [& body]
  `(daemon-future-call (^{:once true} fn* [] ~@body)))

(defmulti handle "[*state event]" (fn [*state e] (::type e)))

(defmethod handle :default [_ e]
  (prn e))

(defrecord MapEventHandler [*state]
  IFn
  (invoke [_ e]
    (handle *state e)))
