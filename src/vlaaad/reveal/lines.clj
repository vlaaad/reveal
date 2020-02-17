(ns vlaaad.reveal.lines)

(defn- scan-cursor [cursor lines row-direction col-direction]
  (loop [[row col] cursor]
    (let [last-col (dec (count (get lines row)))
          next-col (condp = col-direction
                     inc (max (col-direction col) 0)
                     dec (min (col-direction col) last-col))]
      (if (<= 0 next-col last-col)
        [row next-col]
        (let [last-row (dec (count lines))
              next-row (condp = row-direction
                         inc (max (row-direction row) 0)
                         dec (min (row-direction row) last-row))]
          (when (<= 0 next-row last-row)
            (recur [next-row (condp = col-direction
                               inc -1
                               dec (count (lines next-row)))])))))))

(defn scan
  ([lines row direction pred]
   (let [last-row (dec (count lines))]
     (loop [row row]
       (let [next-row (direction row)]
         (when (<= 0 next-row last-row)
           (if (pred (lines next-row))
             next-row
             (recur next-row)))))))
  ([lines cursor row-direction col-direction pred]
   (loop [cursor cursor]
     (when-let [next-cursor (scan-cursor cursor lines row-direction col-direction)]
       (if (pred (get-in lines next-cursor))
         next-cursor
         (recur next-cursor))))))
