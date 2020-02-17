(ns vlaaad.reveal.cursor
  (:refer-clojure :exclude [min max]))

(defn before? [a b]
  (neg? (compare a b)))

(defn row [cursor]
  (nth cursor 0))

(defn col [cursor]
  (nth cursor 1))

(defn min [a b]
  (if (before? a b) a b))

(defn max [a b]
  (if (before? a b) b a))

(defn cursor? [x]
  (and (vector? x)
       (int? (row x))
       (int? (col x))))