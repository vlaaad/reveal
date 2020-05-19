(ns vlaaad.reveal.action
  (:require [clojure.core.protocols :as p]
            [clojure.datafy]
            [clojure.set :as set]
            [clojure.reflect :as reflect]
            [vlaaad.reveal.stream :as stream]
            [vlaaad.reveal.style :as style]
            [clojure.spec.alpha :as s])
  (:import [clojure.lang IDeref]
           [java.awt Desktop]
           [java.net URI URL]
           [java.io File]
           [java.lang.reflect Field Modifier]
           [java.beans Introspector PropertyDescriptor]))

(defonce ^:private *registry
  (atom {:actions (sorted-map-by (fn [[^String label-a id-a] [^String label-b id-b]]
                                   (let [n (.compareToIgnoreCase label-a label-b)]
                                     (if (zero? n)
                                       (compare id-a id-b)
                                       n))))
         :keys {}}))

(defn- unregister [registry id]
  (let [key (get-in registry [:keys id] ::not-found)]
    (cond-> registry
            (not= key ::not-found)
            (-> (update :keys dissoc id)
                (update :actions dissoc key)))))

(defn- register [registry action]
  (let [{:keys [id label]} action
        key [label id]]
    (-> registry
        (unregister id)
        (assoc-in [:keys id] key)
        (assoc-in [:actions key] action))))

(s/def ::id any?)

(s/def ::label string?)

(s/def ::check
  (s/fspec :args (s/cat :val any? :ann (s/nilable map?))
           :ret (s/nilable fn?)))

(s/def ::action
  (s/keys :req-un [::id ::check ::label]))

(s/fdef register!
  :args (s/cat :action ::action)
  :ret ::id)

(defn register! [action]
  (swap! *registry register action)
  (:id action))

(defn- check [action val ann]
  (try
    (let [command ((:check action) val ann)]
      (when (map? command)
        (into action command)))
    (catch Exception _)))

(defn collect [[val ann]]
  (into [] (keep #(check % val ann)) (vals (:actions @*registry))))

(register!
  {:id ::nav
   :label "Navigate"
   :check (fn [x ann]
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
                    (not= key ::not-found) {:invoke #(p/nav coll key x)
                                            :form (list 'nav key)}
                    (not= val ::not-found) {:invoke #(p/nav coll x val)
                                            :form (list 'nav x)})))))})


(register!
  {:id ::show
   :label "Show actual value"
   :check (fn [val ann]
            (when (::stream/hidden ann)
              {:invoke (constantly val)
               :form val}))})

(register!
  {:id ::java
   :label "Java bean"
   :check (fn [x _]
            (when (some? x)
              {:form (list 'bean x)
               :invoke (fn []
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
                                                            (stream/raw-string name {:fill ::style/symbol-color}))
                                                          stream/separator
                                                          (->> kinds
                                                               (map (fn [kind]
                                                                      (stream/as kind
                                                                        (stream/raw-string (.getSimpleName (class kind))
                                                                                           {:fill ::style/util-color}))))
                                                               (interpose stream/separator)))
                                                        (stream/horizontal
                                                          (stream/raw-string "  " {:selectable false})
                                                          (stream/stream value))))))))]
                           (stream/just
                             (stream/sequential sorted))))}))})

(register!
  {:id ::reflect
   :label "Reflect"
   :check (fn [v _]
            (when (class? v)
              {:invoke #(reflect/reflect v)
               :form (list 'reflect v)}))})

(register!
  {:id ::deref
   :label "Deref"
   :check (fn [v _]
            (when (instance? IDeref v)
              {:invoke #(deref v)
               :form (stream/just
                       (stream/horizontal
                         (stream/raw-string "@" {:fill ::style/symbol-color})
                         (stream/stream v)))}))})

(register!
  {:id ::meta
   :label "Meta"
   :check (fn [v _]
            (when-let [m (meta v)]
              {:invoke (constantly m)
               :form (list 'meta v)}))})

(register!
  {:id ::browser
   :label "Browse"
   :check (fn [v _]
            (cond
              (instance? URI v)
              {:invoke #(deref (future (.browse (Desktop/getDesktop) v)))
               :form (list 'browse v)}

              (instance? URL v)
              {:invoke #(deref (future (.browse (Desktop/getDesktop) (.toURI ^URL v))))
               :form (list 'browse v)}

              (instance? File v)
              {:invoke #(deref (future (.browse (Desktop/getDesktop) (.toURI ^File v))))
               :form (list 'browse v)}))})

(register!
  {:id ::vector
   :label "Vec"
   :check (fn [v _]
            (when (and v (.isArray (class v)))
              {:invoke #(vec v)
               :form (list 'vec v)}))})

(register!
  {:id ::as-table
   :label "View as table"
   :check (fn [v _]
            (when (and (some? v)
                       (not (string? v))
                       (seqable? v))
              {:invoke #(stream/just (stream/table v))
               :form (list 'table v)}))})