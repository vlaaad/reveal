(ns vlaaad.reveal.event
  (:import [java.util.concurrent Executors ThreadFactory]))

(def daemon-executor
  (let [*counter (atom 0)
        factory (reify ThreadFactory
                  (newThread [_ runnable]
                    (doto (Thread. runnable (str "reveal-agent-pool-" (swap! *counter inc)))
                      (.setDaemon true))))]
    (Executors/newCachedThreadPool factory)))

(defn- not-found-handler [e]
  (prn (dissoc e :state)))

(defn handler [e]
  ((::handler e not-found-handler) e))