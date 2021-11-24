(ns vlaaad.reveal.event
  (:import [java.util.concurrent Executors ThreadFactory ExecutorService ScheduledExecutorService]
           [clojure.lang IFn]))

(def ^:private ^ThreadFactory daemon-thread-factory
  (let [*counter (atom 0)]
    (reify ThreadFactory
      (newThread [_ runnable]
        (doto (Thread. runnable (str "reveal-agent-pool-" (swap! *counter inc)))
          (.setDaemon true))))))

(def ^ExecutorService daemon-executor
  (Executors/newCachedThreadPool daemon-thread-factory))

(def ^ScheduledExecutorService daemon-scheduler
  (Executors/newSingleThreadScheduledExecutor daemon-thread-factory))

(defn daemon-future-call [f]
  (.submit daemon-executor ^Callable (bound-fn* f)))

(defmacro daemon-future [& body]
  `(daemon-future-call (^{:once true} fn* [] ~@body)))

(defmulti handle ::type)

(defmethod handle :default [e] (prn e) identity)

(defrecord MapEventHandler [*state]
  IFn
  (invoke [_ e]
    (swap! *state (handle e))
    nil))
