(ns e04-tap-to-table
  (:require [vlaaad.reveal.ext :as rx]))

;; Evaluating the following form will return a value with `view` action that
;; will always show last tapped value in a table. This view guesses columns
;; by only looking at the first item in a coll, so it might not show the whole
;; tapped value.

(let [last-tap (atom nil)]
  (add-tap #(reset! last-tap %))
  (rx/view-as-is
    {:fx/type rx/observable-view
     :ref last-tap
     :fn (fn [x]
           (if (or (nil? x) (string? x) (not (seqable? x)))
             {:fx/type rx/table-view
              :items [x]
              :columns [{:fn identity :header 'value}]}
             (let [head (first x)]
               {:fx/type rx/table-view
                :items x
                :columns (cond
                           (map? head) (for [k (keys head)] {:header k :fn #(get % k)})
                           (map-entry? head) [{:header 'key :fn key} {:header 'val :fn val}]
                           (indexed? head) (for [i (range (count head))] {:header i :fn #(nth % i)})
                           :else [{:header 'item :fn identity}])})))}))
