(ns vlaaad.reveal.search
  (:import [clojure.lang MapEntry]))

(defn- rank [^String s ^String q]
  (let [q-len (.length q)
        s-len (.length s)]
    (loop [qi 0
           si 0
           rank 0]
      (if (= qi q-len)
        rank
        (let [ch (Character/toLowerCase (.charAt q qi))
              next-si (loop [i si]
                        (cond
                          (= i s-len) nil
                          (= ch (Character/toLowerCase (.charAt s i))) (inc i)
                          :else (recur (inc i))))]
          (when next-si
            (recur (inc qi) next-si (+ rank (dec (- next-si si))))))))))

(defn select
  ([xs q]
   (select xs q str))
  ([xs q str]
   (->> xs
        (into [] (keep (fn [x]
                         (let [rank (rank (str x) q)]
                           (when rank (MapEntry. x rank))))))
        (sort-by val)
        (mapv key))))
