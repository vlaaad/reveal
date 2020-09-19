(ns vlaaad.reveal.action
  (:require [clojure.datafy :as d]
            [vlaaad.reveal.stream :as stream]
            [clojure.spec.alpha :as s]
            [clojure.core.specs.alpha :as specs])
  (:import [clojure.lang IDeref]
           [java.awt Desktop]
           [java.net URI URL]
           [java.io File]
           [java.lang.reflect Field Modifier]
           [java.beans Introspector PropertyDescriptor]))

(defonce ^:private *registry
  (atom {}))

(defn register! [id check]
  (swap! *registry assoc id check)
  id)

(s/def ::id qualified-keyword?)

(s/fdef def
  :args (s/cat :id ::id
               :bindings (s/every ::specs/binding-form :kind vector? :min-count 1 :max-count 2)
               :body (s/+ any?))
  :ret ::id)

(defmacro defaction [id bindings & body]
  (let [fn-bindings (case (count bindings)
                      1 (conj bindings (gensym "_"))
                      2 bindings)]
    `(register! ~id (fn ~(symbol (name id)) ~fn-bindings ~@body))))

(defn collect [annotated-value]
  (let [{:keys [value annotation]} annotated-value]
    (->> @*registry
         (keep (fn [[id check]]
                 (try
                   (when-let [f (check value annotation)]
                     (let [label (name id)]
                       {:id id
                        :label label
                        :form (stream/as-is
                                (stream/horizontal
                                  (stream/raw-string "(" {:fill :util})
                                  (stream/raw-string label {:fill :symbol})
                                  stream/separator
                                  (stream/stream value annotation)
                                  (stream/raw-string ")" {:fill :util})))
                        :invoke f}))
                   (catch Exception _))))
         (sort-by :label)
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

(defaction ::view:value [x ann]
  (when (::stream/hidden ann)
    (constantly x)))

(defaction ::java-bean [x]
  (when (some? x)
    (fn []
      (let [props (->> x
                       class
                       (Introspector/getBeanInfo)
                       (.getPropertyDescriptors)
                       (keep
                         (fn [^PropertyDescriptor descriptor]
                           (when-let [read-meth (.getReadMethod descriptor)]
                             (.setAccessible read-meth true)
                             [(.getName descriptor)
                              (.invoke read-meth x (object-array 0))
                              descriptor]))))
            fields (->> x
                        (class)
                        (iterate #(.getSuperclass ^Class %))
                        (take-while some?)
                        (mapcat #(.getDeclaredFields ^Class %))
                        (remove #(Modifier/isStatic (.getModifiers ^Field %)))
                        (keep
                          (fn [^Field field]
                            (try
                              (.setAccessible field true)
                              [(.getName field)
                               (.get field x)
                               field]
                              (catch Throwable _
                                nil)))))
            sorted (->> (concat fields props)
                        (group-by (juxt first second))
                        (sort-by ffirst)
                        (map (fn [[[name value] xs]]
                               (let [kinds (mapv last xs)]
                                 (stream/as-is
                                   (stream/vertical
                                     (apply
                                       stream/horizontal
                                       (stream/as name
                                         (stream/raw-string name {:fill :symbol}))
                                       stream/separator
                                       (->> kinds
                                            (map (fn [kind]
                                                   (stream/as kind
                                                     (stream/raw-string (.getSimpleName (class kind))
                                                                        {:fill :util}))))
                                            (interpose stream/separator)))
                                     (stream/horizontal
                                       (stream/raw-string "  " {:selectable false})
                                       (stream/stream value))))))))]
        (stream/as-is
          (stream/vertically sorted))))))

(defaction ::deref [v]
  (when (instance? IDeref v)
    #(deref v)))

(defaction ::meta [v]
  (when-let [m (meta v)]
    (constantly m)))

(defaction ::browse:external [v]
  ;; todo don't open result panel
  (cond
    (instance? URI v)
    #(deref (future (.browse (Desktop/getDesktop) v)))

    (instance? URL v)
    #(deref (future (.browse (Desktop/getDesktop) (.toURI ^URL v))))

    (and (instance? File v) (.exists ^File v))
    #(deref (future (.browse (Desktop/getDesktop) (.normalize (.toURI ^File v)))))

    (and (string? v) (re-matches #"^https?://.+" v))
    #(deref (future (.browse (Desktop/getDesktop) (URI. v))))))

(defaction ::vec [v]
  (when (and v (.isArray (class v)))
    #(vec v)))