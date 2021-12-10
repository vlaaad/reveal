(ns e07-timbre-tap-appender
  (:require [taoensso.timbre :as log]
            [vlaaad.reveal :as r]))

;; The goal is to get timbre's logs into the reveal window.
;; A simple way to achieve this is to `tap>` the messages.
;; That can be achieved by adding a custom appender.
;; Timbre generates the log message from a map that contains
;; various metadata. We can keep that around and still display
;; just the message by using `rx/as`.

(log/merge-config!
  {:appenders
   {:println {:enabled? false}
    :reveal {:enabled? true
             :fn (fn [data]
                   (tap> (r/submit
                           (r/as data
                                 (r/raw-string
                                   (format "[%1$tH:%1$tM:%1$tS.%1$tL %2$s:%3$s]: %4$s"
                                           (:instant data)
                                           (:?ns-str data)
                                           (:?line data)
                                           @(:msg_ data))
                                   {:fill ({:info :symbol
                                            :report :symbol
                                            :warn "#db8618"
                                            :error :error
                                            :fatal :error}
                                           (:level data)
                                           :util)})))))}}})

(comment
  (log/info "hi")
  (log/warn "bad")
  (log/error "very bad" (ex-info "nope" {:x 1})))
