(ns vlaaad.reveal.ns
  (:import [java.io FileNotFoundException]))

(defmacro when-exists [ns-sym & body]
  `(try
     ;; not our problem
     (binding [*warn-on-reflection* false]
       (require '~ns-sym))
     ~@body
     (catch FileNotFoundException _#)))
