(ns e06-submit-with-form
  (:require [vlaaad.reveal :as r]))

;; This example shows how to create a custom Reveal UI that allows submitting
;; evaluation results with code forms that produced these results (similar to
;; REBL's submit function)

(def ui (r/ui))

(defn submit-with-form [form value]
  (ui (r/vertical
        (r/as form (r/raw-string (pr-str form) {:fill :util}))
        (r/horizontal
          (r/raw-string "=>" {:fill :util})
          r/separator
          (r/stream value)))))

(comment
  ;; example usage
  (submit-with-form '(+ 1 2 3) 6))

;; reveal shows:
;; (+ 1 2 3)
;; => 6