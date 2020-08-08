(ns vlaaad.reveal.action
  (:require [clojure.datafy :as d]
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

(defmacro def
  "Define action for execution in the context of some selected value

  When user requests a context menu on a selected value, all actions are
  evaluated. If action body returns 0-arg fn, the action is shown in the
  context menu, and the function will be invoked when user selects the action
  for execution. Any other evaluation results, including thrown exceptions, are
  ignored.

  `id` is a ns-qualified keyword identifying this action
  `bindings` is a bindings vector that can have either 1 or 2 args, where first
  argument is a selected value and second is an annotation supplied by reveal
  output streaming process
  `body` is an action body that has access to `bindings`, should return 0-arg
  function for action to be available in the context menu"
  [id bindings & body]
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

(vlaaad.reveal.action/def ::datafy [x]
  (let [d (d/datafy x)]
    (when-not (= d x)
      (constantly d))))

(vlaaad.reveal.action/def ::nav [x {:vlaaad.reveal.nav/keys [coll key val]
                                    :or {key ::not-found
                                         val ::not-found}}]
  (let [datafied-coll (d/datafy coll)]
    (when (= datafied-coll coll)
      (cond
        (not= key ::not-found) #(d/nav datafied-coll key x)
        (not= val ::not-found) #(d/nav datafied-coll x val)))))

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