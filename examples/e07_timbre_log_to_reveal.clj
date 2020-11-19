(ns e07-timbre-log-to-reveal
  (:require [taoensso.timbre :as timbre]
            [vlaaad.reveal.ext :as rx]))

;; The goal is to get timbre's logs into the reveal window.
;; A simple way to achieve this is to `tap>` the messages.
;; That can be achieved by adding a custom appender.
;; Timbre generates the log message from a map that contains
;; various metadata. We can keep that around and still display
;; just the message by using `rx/as`.
(timbre/merge-config!
 {:appenders {:reveal {:enabled? true :async? true :min-level nil :rate-limit nil :output-fn timbre/default-output-fn
                       :fn (fn [data] (tap> (rx/stream-as-is (rx/as data (rx/raw-string @(:output_ data))))))}}})

(comment
  (log/info "hi")
  )
