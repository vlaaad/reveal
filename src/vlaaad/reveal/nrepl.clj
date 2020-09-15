(ns vlaaad.reveal.nrepl
  (:require [nrepl.middleware :as middleware]
            [nrepl.middleware.print :as print]
            [nrepl.transport :as transport]
            [clojure.string :as str]
            [vlaaad.reveal.ui :as ui]
            [vlaaad.reveal.stream :as stream]))

(defn- show-output [ui request message]
  (when-let [code (:code request)]
    (when-not (or (str/starts-with? code "(cursive.repl.runtime/")
                  (:done (:status message))
                  (= 0 (:pprint request)))
      (let [{:keys [out value err nrepl.middleware.caught/throwable]
             :or {out ::not-found
                  value ::not-found
                  err ::not-found
                  throwable ::not-found}} message]
        (ui
          (cond
            (not= value ::not-found)
            (stream/as-is
              (stream/as {:request request :message message}
                (stream/vertical
                  (stream/raw-string code {:fill :util})
                  (stream/horizontal
                    (stream/raw-string "=>" {:fill :util})
                    stream/separator
                    (stream/stream value)))))

            (not= out ::not-found)
            (stream/as-is
              (stream/as {:request request :message message}
                (stream/raw-string (str/trim-newline out) {:fill :string})))

            (not= err ::not-found)
            (stream/as-is
              (stream/as {:request request :message message}
                (stream/raw-string (str/trim-newline err) {:fill :error})))

            (not= throwable ::not-found)
            (stream/as-is
              (stream/as {:request request :message message}
                (stream/vertical
                  (stream/raw-string code {:fill :util})
                  (stream/horizontal
                    (stream/raw-string "=>" {:fill :util})
                    stream/separator
                    (stream/as throwable
                      (stream/raw-string
                        (.getSimpleName (class throwable))
                        {:fill :error}))))))

            :else
            {:request request :message message}))))))

(defn- show-tap [ui value]
  (ui
    (stream/as {:tap value}
      (stream/horizontal
        (stream/raw-string "tap>" {:fill :util})
        stream/separator
        (stream/stream value)))))

(defn middleware [f]
  (let [ui (ui/make)]
    (add-tap #(show-tap ui %))
    (fn [request]
      (-> request
          (update :transport (fn [t]
                               (reify transport/Transport
                                 (recv [_] (transport/recv t))
                                 (recv [_ timeout]
                                   (transport/recv t timeout))
                                 (send [this message]
                                   (show-output ui request message)
                                   (transport/send t message)
                                   this))))
          (f)))))

(middleware/set-descriptor! #'middleware
                            {:requires #{#'print/wrap-print}
                             :expects #{"eval"}
                             :handles {}})

