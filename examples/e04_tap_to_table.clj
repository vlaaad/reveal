(ns e04-tap-to-table)

;; Evaluating the following form will make reveal open a view that
;; will always show last tapped value in a table. This view guesses columns
;; by only looking at the first item in a coll, so it might not show the whole
;; tapped value. When the view is closed, it will remove the tap listener

{:vlaaad.reveal/command
 '(open-view
    {:fx/type observable-view
     :subscribe (fn [notify]
                  (add-tap notify)
                  #(remove-tap notify))
     :fn (fn [x]
           (if (or (nil? x) (string? x) (not (seqable? x)))
             {:fx/type table-view
              :items [x]
              :columns [{:fn identity :header 'value}]}
             (let [head (first x)]
               {:fx/type table-view
                :items x
                :columns (cond
                           (map? head) (for [k (keys head)] {:header k :fn #(get % k)})
                           (map-entry? head) [{:header 'key :fn key} {:header 'val :fn val}]
                           (indexed? head) (for [i (range (count head))] {:header i :fn #(nth % i)})
                           :else [{:header 'item :fn identity}])})))})}
