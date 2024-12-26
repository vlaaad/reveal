(ns vlaaad.reveal.memory
  (:require [clojure.string :as str]
            [vlaaad.reveal.action :as action]
            [vlaaad.reveal.stream :as stream]
            [vlaaad.reveal.view :as view])
  (:import [clojure.lang MapEntry]
           [java.lang.reflect Field Modifier]
           [java.util ArrayDeque ArrayList HashMap IdentityHashMap PriorityQueue]
           [org.openjdk.jol.util ObjectUtils SimpleIdentityHashSet]
           [org.openjdk.jol.vm VM]))

(set! *warn-on-reflection* true)

(defn tree
  "Returns struct-of-arrays map with the following keys:
    :objects     all reachable objects, the input object is first
    :children    for every reachable object, int array of successor object indices
    :labels      for every reachable object, its parent label, either:
                 - nil for the root object
                 - string if the object is in a field of another object
                 - long if the object is in an array
    :memory      for every reachable object, its total referenced memory in bytes"
  [x]
  (let [visited (doto (SimpleIdentityHashSet.) (.add x))
        class-field-cache (HashMap.)
        deque (doto (ArrayDeque.) (.add x))
        object->index (IdentityHashMap.)
        objects (ArrayList.)
        children (ArrayList.)
        labels (ArrayList.)
        get-object-index (fn [x]
                           (or (.get object->index x)
                               (let [i (.size objects)]
                                 (.put object->index x i)
                                 (.add objects x)
                                 (.add children nil)
                                 (.add labels nil)
                                 i)))]
    (loop []
      (if-some [object (.poll deque)]
        (let [object-index (get-object-index object)
              cl (class object)
              ;; acc is [object-index [label object]]
              acc (into []
                        (keep
                          (fn [e] ;; label+object
                            (when (.add visited (val e))
                              (MapEntry. (get-object-index (val e)) e))))
                        (if (.isArray cl)
                          (if (.isPrimitive (.getComponentType cl))
                            nil
                            (->> object
                                 (eduction
                                   (map-indexed vector)
                                   (keep (fn [[i item]]
                                           (when (some? item)
                                             (MapEntry. i item)))))))
                          (let [fields (or (.get class-field-cache cl)
                                           (let [fields (->> cl
                                                             (iterate #(.getSuperclass ^Class %))
                                                             (eduction
                                                               (take-while some?)
                                                               (mapcat (fn [^Class c]
                                                                         (.getDeclaredFields c)))
                                                               (remove (fn [^Field f]
                                                                         (or (Modifier/isStatic (.getModifiers f))
                                                                             (.isPrimitive (.getType f))))))
                                                             vec)]
                                             (.put class-field-cache cl fields)
                                             fields))]
                            (->> fields
                                 (eduction
                                   (keep (fn [^Field f]
                                           (when-some [v (ObjectUtils/value object f)]
                                             (MapEntry. (.getName f) v)))))))))]
          (.set children object-index (int-array (count acc) (mapv key acc)))
          (run! #(.set labels (key %) (key (val %))) acc)
          (run! #(.add deque (val (val %))) acc)
          (recur))
        (let [memory (long-array (.size objects))
              vm (VM/current)]
          (loop [i (dec (.size objects))]
            (when-not (neg? i)
              (let [own (.sizeOf vm (.get objects i))]
                (aset memory i ^long (reduce + own (->Eduction (map #(aget memory %)) (.get children i)))))
              (recur (dec i))))
          {:objects (.toArray objects)
           :children (.toArray children ^"[[I" (make-array Integer/TYPE 0 0))
           :labels (.toArray labels)
           :memory memory})))))

(defn bytes-to-human-readable [bytes]
  (let [kb 1024
        mb (* kb 1024)
        fmt #(str/replace (format "%5.2f" %) "," ".")]
    (cond
      (>= bytes mb) (str (fmt (double (/ bytes mb))) "mb")
      (>= bytes kb) (str (fmt (double (/ bytes kb))) "kb")
      :else (str bytes "b"))))

(defn- top-n-by [n f coll]
  (let [[xs truncated] (if (<= (count coll) n)
                         [coll 0]
                         (let [pq (PriorityQueue. #(compare (f %1) (f %2)))]
                           [(reduce
                              (fn [^PriorityQueue pq v]
                                (if (< (.size pq) n)
                                  (doto pq (.offer v))
                                  (if (> (f v) (f (.peek pq)))
                                    (doto pq (.poll) (.offer v))
                                    pq)))
                              pq
                              coll)
                            (- (count coll) n)]))]
    (with-meta (vec (sort-by (comp - f) xs)) {:truncated truncated})))

(action/defaction ::action/memory:tree [x]
  (when (some? x)
    (fn []
      (try
        (let [{:keys [^"[Ljava.lang.Object;" objects
                      ^"[[I" children
                      ^"[Ljava.lang.Object;" labels
                      ^"[J" memory]} (tree x)]
          {:fx/type view/tree-view
           :root 0
           :render #(if (:truncated %)
                      (stream/raw-string (str "..." (- (count (:truncated %)) 100) " more") {:fill :util})
                      (stream/horizontal
                        (let [v (aget labels %)]
                          (if v
                            (stream/raw-string v {:fill :symbol})
                            (stream/raw-string "<root>" {:fill :util})))
                        stream/separator
                        (stream/raw-string (bytes-to-human-readable (aget memory %)) {:fill :scalar})
                        stream/separator
                        (stream/stream (aget objects %))))
           :valuate #(when-not (:truncated %) (aget ^"[Ljava.lang.Object;" objects %))
           :branch? #(or (:truncated %) (pos? (count (aget children %))))
           :children #(if (:truncated %)
                        (vec (drop 100 (sort-by (comp - (fn [i] (aget memory i))) (:truncated %))))
                        (let [is (aget children %)
                              ret (top-n-by 100 (fn [i] (aget memory i)) is)]
                          (if (pos? (:truncated (meta ret)))
                            (conj ret {:truncated is})
                            ret)))})
        (catch Exception e
          (if (str/includes? (ex-message e) "-Djol.magicFieldOffset=true")
            (stream/as "-Djol.magicFieldOffset=true" (stream/raw-string "Cannot analyze memory, try with the JVM option:\n-Djol.magicFieldOffset=true" {:fill :error}))
            (throw e)))))))
