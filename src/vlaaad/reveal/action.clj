(ns vlaaad.reveal.action
  (:require [clojure.core.protocols :as p]
            [clojure.datafy]
            [clojure.set :as set]
            [clojure.reflect :as reflect]
            [vlaaad.reveal.stream :as stream]
            [vlaaad.reveal.style :as style]
            [clojure.string :as str])
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

(defn register! [action]
  (swap! *registry register action)
  (:id action))

(defn- check [action values]
  (try
    (let [f ((:check action) values)]
      (when (ifn? f)
        (assoc action :invoke f)))
    (catch Exception _)))

(defn collect [values]
  (let [registry @*registry]
    (into []
          (keep #(check % values))
          (vals (:actions registry)))))

(register!
  {:id ::nav
   :label "Navigate"
   :check (fn [values]
            (when (<= 2 (count values))
              (let [[x ann] (peek values)
                    coll (p/datafy (first (peek (pop values))))
                    c (class coll)]
                (when (or (instance? (:on-interface p/Navigable) coll)
                          (contains? (meta coll) `p/nav)
                          (seq (set/intersection (disj (supers c) Object)
                                                 (set (keys (:impls p/Navigable))))))
                  (let [{:vlaaad.reveal.nav/keys [key val index]
                         :or {key ::not-found
                              val ::not-found}} ann]
                    (cond
                      index #(p/nav coll index x)
                      (not= key ::not-found) #(p/nav coll key x)
                      (not= val ::not-found) #(p/nav coll x val)))))))})

(register!
  {:id ::show
   :label "Show"
   :check (fn [vals+anns]
            (when-let [[v a] (peek vals+anns)]
              (when (::stream/hidden a)
                (constantly v))))})

(register!
  {:id ::inspect
   :label "Inspect"
   :check (fn [vals+anns]
            (when-some [x (first (peek vals+anns))]
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
                                         (let [kinds (mapv last xs)
                                               row [(stream/as (if (= 1 (count kinds)) (first kinds) kinds)
                                                      (stream/raw-string name {:fill ::style/object-color}))
                                                    value
                                                    (stream/as kinds
                                                      (stream/raw-string
                                                        (str "("
                                                             (str/join ", " (map #(-> % class .getSimpleName) kinds))
                                                             ")")
                                                        {:fill ::style/util-color}))]]
                                           (stream/as row
                                             (stream/items row))))))]
                  (stream/as sorted
                    (stream/sequential sorted))))))})

(register!
  {:id ::reflect
   :label "Reflect"
   :check (fn [vals+anns]
            (when-let [[v] (peek vals+anns)]
              (when (class? v)
                #(reflect/reflect v))))})

(register!
  {:id ::deref
   :label "Deref"
   :check (fn [vals+anns]
            (when-let [[v] (peek vals+anns)]
              (when (instance? IDeref v)
                #(deref v))))})

(register!
  {:id ::meta
   :label "Meta"
   :check (fn [vals+anns]
            (when-let [m (meta (first (peek vals+anns)))]
              (constantly m)))})

(register!
  {:id ::browser
   :label "Browse"
   :check (fn [vals+anns]
            (when-let [v (first (peek vals+anns))]
              (cond
                (instance? URI v)
                #(deref (future (.browse (Desktop/getDesktop) v)))

                (instance? URL v)
                #(deref (future (.browse (Desktop/getDesktop) (.toURI ^URL v))))

                (instance? File v)
                #(deref (future (.browse (Desktop/getDesktop) (.toURI ^File v)))))))})

(register!
  {:id ::vector
   :label "To vector"
   :check (fn [vals+anns]
            (when-let [v (first (peek vals+anns))]
              (when (.isArray (class v))
                #(vec v))))})
