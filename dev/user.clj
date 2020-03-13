(ns user
  (:import [clojure.lang PersistentQueue]
           [java.util UUID HashSet HashMap ArrayList Date LinkedList]
           [java.sql Timestamp]
           [java.time Instant LocalTime LocalDateTime ZoneOffset]))

(def interesting-values
  {:numbers {:integer (int 1)
             :long (long 2)
             :double (double 1.2)
             :float (float 3.14)
             :big-integer (biginteger 3)
             :big-decimal (bigdec 4)
             :ratio 3/4
             :short (short 5)
             :byte (byte 6)
             :big-int (bigint 7)}
   :scalars {:boolean true
             :keyword :keyword
             :q-keyword :qualified/keyword
             :symbol 'symbol}
   :strings {:string "string"
             :character \c
             :re #"(\s)+"}
   :queue (conj (PersistentQueue/EMPTY) 1 2 3)
   :bytes (.getBytes "bytes")
   :uuid (UUID/randomUUID)
   :colls {:java {:set (doto (HashSet.) (.add 1) (.add 2) (.add 3))
                  :map (doto (HashMap.) (.put :foo "bar"))
                  :array-list (doto (ArrayList.) (.add 1) (.add 2) (.add 3))
                  :linked-list (doto (LinkedList.) (.add '/) (.add 1) (.add :a))}
           :clojure {:set #{:a :b :c}
                     :map {:foo "bar"}
                     :list '(+ 1 2 3)
                     :syntax-quoted `(+ 1 2 3)
                     :range (range 5 10)
                     :vec [1 2 3]}}
   :refs {:var #'inc
          :future (future 10)
          :delay (delay 20)
          :promise (doto (promise) (deliver 1))
          :atom (atom :atom)
          :agent (agent "agent")
          :ref (ref 1)}
   :dates {:date (Date. 0)
           :timestamp (Timestamp. 0)
           :instant (Instant/ofEpochMilli 0)
           :local-time (LocalTime/ofNanoOfDay 0)
           :local-date-time (LocalDateTime/ofEpochSecond 0 0 ZoneOffset/UTC)}})
