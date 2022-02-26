(ns vlaaad.reveal.action
  (:require [clojure.datafy :as d]
            [vlaaad.reveal.stream :as stream]
            [clojure.spec.alpha :as s]
            [clojure.core.specs.alpha :as specs]
            [vlaaad.reveal.event :as event]
            [lambdaisland.deep-diff2 :as diff])
  (:import [clojure.lang IDeref]
           [java.awt Desktop]
           [java.net URI URL]
           [java.io File]
           [java.util.concurrent Future]))

(defonce ^:private *registry
  (atom {}))

(defn register! [id check]
  (swap! *registry assoc id check)
  id)

(s/def ::id qualified-keyword?)

(s/fdef defaction
  :args (s/cat :id ::id
               :bindings (s/every ::specs/binding-form :kind vector? :min-count 1 :max-count 2)
               :body (s/+ any?))
  :ret ::id)

(defmacro defaction [id bindings & body]
  (let [name (symbol (str (name id) "-action-body"))]
    `(register! ~id (fn ~name ~@(case (count bindings)
                                  1 `((~bindings ~@body)
                                      ([~'value ~'annotation] (~name ~'value)))
                                  2 `(([~'value] (~name ~'value nil))
                                      (~bindings ~@body)))))))

(defn collect [annotated-value]
  (let [{:keys [value annotation]} annotated-value
        actions (->> @*registry
                     (keep (fn [[id check]]
                             (try
                               (when-let [f (check value annotation)]
                                 (let [label (name id)]
                                   {:id id
                                    :label label
                                    :form (stream/horizontal
                                            (stream/raw-string "(" {:fill :util})
                                            (stream/raw-string label {:fill :symbol})
                                            stream/separator
                                            (stream/stream value annotation)
                                            (stream/raw-string ")" {:fill :util}))
                                    :invoke f}))
                               (catch Exception _)))))
        freqs (->> actions (map :label) frequencies)]
    (->> actions
         (sort-by (juxt :label :id))
         (map #(cond-> % (< 1 (freqs (:label %))) (assoc :label (str (symbol (:id %))))))
         (into []))))

(defaction ::datafy [x]
  (let [d (d/datafy x)]
    (when-not (= d x)
      (constantly d))))

(defaction ::nav [x {:vlaaad.reveal.nav/keys [coll key val]
                     :or {key ::not-found
                          val ::not-found}}]
  (let [datafied-coll (d/datafy coll)]
    (when (= datafied-coll coll)
      (cond
        (not= key ::not-found) #(d/nav datafied-coll key x)
        (not= val ::not-found) #(d/nav datafied-coll x val)))))

(defaction ::deref [v]
  (when (or (instance? IDeref v)
            (instance? Future v))
    #(deref v)))

(defaction ::meta [v]
  (when-let [m (meta v)]
    (constantly m)))

(defaction ::browse:external [v]
  (cond
    (instance? URI v)
    (with-meta #(deref (future (.browse (Desktop/getDesktop) v)))
               {:vlaaad.reveal.ui/ignore-action-result true})

    (instance? URL v)
    (recur (.toURI ^URL v))

    (and (instance? File v) (.exists ^File v))
    (recur (.normalize (.toURI ^File v)))

    (and (string? v) (re-matches #"^https?://.+" v))
    (recur (URI. v))))

(defaction ::vec [v]
  (when (and v (.isArray (class v)))
    #(vec v)))

(defaction ::diff [x]
  (cond
    (and (vector? x) (= 2 (count x)))
    #(apply diff/diff x)

    (and (map? x) (contains? x :expected) (contains? x :actual))
    #(let [{:keys [expected actual]} x]
       (if (and (sequential? expected)
                (= '= (first expected))
                (sequential? actual)
                (= 'not (first actual))
                (sequential? (second actual))
                (= '= (first (second actual)))
                (= 3 (bounded-count 4 (second actual))))
         (apply diff/diff (drop 1 (second actual)))
         (diff/diff expected actual)))))

(defn execute [id x ann]
  (event/daemon-future
    (if-let [action (@*registry id)]
      (if-let [invoke (action x ann)]
        (invoke)
        (throw (ex-info "Action unavailable" {:action id :value x :annotation ann})))
      (throw (ex-info "Action does not exist" {:action id})))))