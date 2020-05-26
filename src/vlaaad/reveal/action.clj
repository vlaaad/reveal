(ns vlaaad.reveal.action
  (:require [clojure.core.protocols :as p]
            [clojure.datafy]
            [clojure.set :as set]
            [clojure.reflect :as reflect]
            [vlaaad.reveal.stream :as stream]
            [vlaaad.reveal.style :as style]
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

(defmacro def [id bindings & body]
  (let [fn-bindings (case (count bindings)
                      1 (conj bindings (gensym "_"))
                      2 bindings)]
    `(register! ~id (fn ~fn-bindings ~@body))))

(defn collect [annotated-value]
  (->> @*registry
       (keep (fn [[id check]]
               (try
                 (when-let [f (check (stream/value annotated-value)
                                     (stream/annotation annotated-value))]
                   (let [label (name id)]
                     {:id id
                      :label label
                      :form (stream/just
                              (stream/horizontal
                                (stream/raw-string "(" {:fill style/util-color})
                                (stream/raw-string label {:fill style/symbol-color})
                                stream/separator
                                (stream/stream annotated-value)
                                (stream/raw-string ")" {:fill style/util-color})))
                      :invoke f}))
                 (catch Exception _))))
       (sort-by :label)
       (into [])))

(vlaaad.reveal.action/def ::nav [x ann]
  ;; todo what if datafication is a different coll?values
  ;; todo: some stuff might have custom datafies, need to suggest them too
  (let [coll (p/datafy (:vlaaad.reveal.nav/coll ann))
        c (class coll)]
    (when (or (instance? (:on-interface p/Navigable) coll)
              (contains? (meta coll) `p/nav)
              (seq (set/intersection (disj (supers c) Object)
                                     (set (keys (:impls p/Navigable))))))
      (let [{:vlaaad.reveal.nav/keys [key val]
             :or {key ::not-found
                  val ::not-found}} ann]
        (cond
          (not= key ::not-found) #(p/nav coll key x)
          (not= val ::not-found) #(p/nav coll x val))))))

(vlaaad.reveal.action/def ::view:value [x ann]
  (when (::stream/hidden ann)
    (constantly x)))

(vlaaad.reveal.action/def ::java-bean [x]
  (when (some? x)
    (fn []
      (let [props (->> x
                       class
                       (Introspector/getBeanInfo)
                       (.getPropertyDescriptors)
                       (keep
                         (fn [^PropertyDescriptor descriptor]
                           (when-let [read-meth (.getReadMethod descriptor)]
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
                                 (stream/just
                                   (stream/vertical
                                     (apply
                                       stream/horizontal
                                       (stream/as name
                                         (stream/raw-string name {:fill style/symbol-color}))
                                       stream/separator
                                       (->> kinds
                                            (map (fn [kind]
                                                   (stream/as kind
                                                     (stream/raw-string (.getSimpleName (class kind))
                                                                        {:fill style/util-color}))))
                                            (interpose stream/separator)))
                                     (stream/horizontal
                                       (stream/raw-string "  " {:selectable false})
                                       (stream/stream value))))))))]
        (stream/just
          (stream/sequential sorted))))))

(vlaaad.reveal.action/def ::reflect [v]
  (when (class? v)
    #(reflect/reflect v)))

(vlaaad.reveal.action/def ::deref [v]
  (when (instance? IDeref v)
    #(deref v)))

(vlaaad.reveal.action/def ::meta [v]
  (when-let [m (meta v)]
    (constantly m)))

(vlaaad.reveal.action/def ::browse [v]
  ;; todo don't open result panel
  (cond
    (instance? URI v)
    #(deref (future (.browse (Desktop/getDesktop) v)))

    (instance? URL v)
    #(deref (future (.browse (Desktop/getDesktop) (.toURI ^URL v))))

    (instance? File v)
    #(deref (future (.browse (Desktop/getDesktop) (.toURI ^File v))))))

(vlaaad.reveal.action/def ::vec [v]
  (when (and v (.isArray (class v)))
    #(vec v)))