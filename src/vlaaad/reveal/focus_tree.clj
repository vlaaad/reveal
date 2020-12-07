(ns vlaaad.reveal.focus-tree
  (:refer-clojure :exclude [next])
  (:require [vlaaad.reveal.ext :as rx])
  (:import [java.util List]))

(defn add [tree parent-id id]
  (let [{::keys [order depth] :or {order []}} tree
        parent-depth (get depth parent-id -1)
        parent-index (.indexOf ^List order parent-id)
        insert-index (inc (+ parent-index
                             (->> (subvec order (inc parent-index))
                                  (take-while #(< parent-depth (get depth %)))
                                  count)))]
    {::id id
     ::index insert-index
     ::order (into (subvec order 0 insert-index)
                   (cons id (subvec order insert-index)))
     ::depth (assoc depth id (inc parent-depth))}))

(defn has-prev? [tree]
  (pos? (::index tree)))

(defn has-next? [tree]
  (< (::index tree) (dec (count (::order tree)))))

(defn focus-index [tree index]
  (assoc tree ::index index ::id ((::order tree) index)))

(defn focus-next [tree]
  (let [{::keys [index order]} tree
        new-index (inc index)]
    (cond-> tree (< new-index (count order)) (focus-index new-index))))

(defn focus-prev [tree]
  (let [{::keys [index]} tree
        new-index (dec index)]
    (cond-> tree (> new-index -1) (focus-index new-index))))

(defn close [tree]
  (let [{::keys [id order depth index]} tree
        id-depth (get depth id)
        splice-count (->> (subvec order (inc index))
                          (take-while #(< id-depth (get depth %)))
                          count)
        new-order (into (subvec order 0 index)
                        (subvec order (inc index)))
        new-depth (reduce
                    (fn [acc id]
                      (update acc id dec))
                    (dissoc depth id)
                    (subvec order (inc index) (+ (inc index) splice-count)))
        new-focus-index (max 0
                             (min (cond-> index (zero? splice-count) dec)
                                  (dec (count new-order))))]
    (when (pos? (count new-order))
      {::id (new-order new-focus-index)
       ::index new-focus-index
       ::order new-order
       ::depth new-depth})))
