(ns vlaaad.reveal.stream
  (:refer-clojure :exclude [newline])
  (:require [vlaaad.reveal.font :as font]
            [vlaaad.reveal.style :as style]
            [clojure.main :as m])
  (:import [clojure.lang Keyword Symbol IPersistentMap IPersistentVector IPersistentSet Fn
                         ISeq MultiFn IRef Var Volatile Namespace IRecord Delay
                         IBlockingDeref TaggedLiteral Reduced ReaderConditional
                         IPersistentCollection BigInt]
           [java.util.regex Pattern]
           [java.io File]
           [java.net URL URI]
           [java.util UUID List Collection RandomAccess Map Set TimeZone Date Calendar ArrayList]
           [clojure.core Eduction]
           [java.text SimpleDateFormat DateFormat]
           [java.time Instant]))

(set! *warn-on-reflection* true)

;; region emitter ops

(defn- =>
  ([] (fn [_ acc] acc))
  ([f] f)
  ([f g]
   (fn [rf acc]
     (let [acc (f rf acc)]
       (if (reduced? acc)
         acc
         (g rf acc)))))
  ([f g h]
   (fn [rf acc]
     (let [acc (f rf acc)]
       (if (reduced? acc)
         acc
         (let [acc (g rf acc)]
           (if (reduced? acc)
             acc
             (h rf acc)))))))
  ([f g h & fs]
   (reduce => (cons f (cons g (cons h fs))))))

(defn- op [op]
  (fn [rf acc]
    (rf acc op)))

(defn- with-value [x ann sf]
  (=> (op {:op ::push-value :value [x ann]})
      sf
      (op {:op ::pop-value})))

(def separator
  (op {:op ::separator}))

(defn- string [str style]
  (op {:op ::string
       :text str
       :style style}))

(defn- block [block-type sf]
  (=> (op {:op ::push-block :block block-type})
      sf
      (op {:op ::pop-block})))

;; endregion

(defn- stream-dispatch [x]
  (or (::type (meta x))
      (class x)))

(defmulti emit stream-dispatch)

(defmulti annotate (fn [x ann sf]
                     (stream-dispatch x)))

(defmethod annotate :default [x ann sf]
  (with-value x ann sf))

(defmethod emit ::hidden [x] x)

(defmethod annotate ::hidden [_ _ sf]
  sf)

(defn stream
  "Streams value using default emitting"
  ([x]
   (stream x nil))
  ([x ann]
   (annotate x ann (emit x))))

(defn just [sf]
  (with-meta sf {::type ::hidden}))

(defn as
  "Streams value using custom sf"
  ([x sf]
   (as x nil sf))
  ([x ann sf]
   (just (with-value x (assoc ann ::hidden true) sf))))

(defn- flush-builder [^StringBuilder builder style]
  (fn [rf acc]
    (let [len (.length builder)]
      (if (pos? len)
        (let [ret ((string (.toString builder) style) rf acc)]
          (.delete builder 0 len)
          ret)
        acc))))

(defn- flush+util [builder style str]
  (=> (flush-builder builder style)
      (string str {:fill ::style/util-color})))

(defn- process-raw [^StringBuilder builder ^String str style]
  (fn [rf acc]
    (let [len (.length str)]
      (loop [i 0
             acc acc]
        (if (or (== i len) (reduced? acc))
          acc
          (let [ch (.charAt str i)]
            (recur
              (inc i)
              (case ch
                \newline ((=> (flush-builder builder style) separator) rf acc)
                \tab (do (.append builder "    ") acc)
                \return ((flush+util builder style "\\r") rf acc)
                \formfeed ((flush+util builder style "\\f") rf acc)
                \backspace ((flush+util builder style "\\b") rf acc)
                (do (.append builder ch) acc)))))))))

(defn- raw [str style]
  (let [builder (StringBuilder.)]
    (=> (process-raw builder str style)
        (flush-builder builder style))))

(defn- escaped [^String str style escape escape-style]
  (fn [rf acc]
    (let [len (.length str)
          builder (StringBuilder.)
          same-style (= style escape-style)]
      (loop [i 0
             acc acc]
        (if (reduced? acc)
          acc
          (if (== i len)
            ((flush-builder builder style) rf acc)
            (let [ch (.charAt str i)
                  esc (escape ch)]
              (recur
                (inc i)
                (if esc
                  (if same-style
                    ((process-raw builder esc style) rf acc)
                    ((=> (flush-builder builder style)
                         (process-raw builder esc escape-style)
                         (flush-builder builder escape-style))
                     rf acc))
                  (case ch
                    \newline ((=> (flush-builder builder style) separator) rf acc)
                    \tab (do (.append builder "    ") acc)
                    \return ((flush+util builder style "\\r") rf acc)
                    \formfeed ((flush+util builder style "\\f") rf acc)
                    \backspace ((flush+util builder style "\\b") rf acc)
                    (do (.append builder ch) acc)))))))))))

(defn raw-string
  ([x]
   (raw-string x {}))
  ([x style]
   (block :paragraph (raw (str x) style))))

(defn escaped-string
  ([x escape]
   (escaped-string x {} escape {}))
  ([x style escape]
   (escaped-string x style escape style))
  ([x style escape escape-style]
   (block :paragraph (escaped (str x) style escape escape-style))))

(defn horizontal [& sfs]
  (block :horizontal (apply => sfs)))

(defn vertical [& sfs]
  (block :vertical (apply => (interpose separator sfs))))

#_(defn- returning-xf [rf]
    (fn
      ([acc]
       (cond-> acc (::return acc) ::return))
      ([acc input]
       (let [ret (rf acc input)]
         (if (reduced? ret)
           (reduced {::return ret})
           ret)))))

#_(defn- through [xf sf]
    (fn [rf acc]
      (let [f (xf (returning-xf rf))
            ret (sf f acc)]
        (if (reduced? ret)
          (if (::return @ret)
            (::return @ret)
            (f @ret))
          (f ret)))))

(defn- streamduce [xf coll]
  (fn [rf acc]
    (transduce xf (fn ([acc] acc) ([acc sf] (sf rf acc))) acc coll)))

(defn entries [m]
  (block :vertical
    (streamduce
      (comp
        (map (fn [e]
               (let [k (key e)
                     v (val e)]
                 (horizontal (stream k {:vlaaad.reveal.nav/val v :vlaaad.reveal.nav/coll m})
                             separator
                             (stream v {:vlaaad.reveal.nav/key k :vlaaad.reveal.nav/coll m})))))
        (interpose separator))
      m)))

(defn- delimited-items [coll]
  (streamduce
    (comp
      (if (set? coll)
        (map
          (fn [x]
            (stream x {:vlaaad.reveal.nav/key x
                       :vlaaad.reveal.nav/coll coll})))
        (map-indexed
          (fn [i x]
            (stream x {:vlaaad.reveal.nav/key i
                       :vlaaad.reveal.nav/coll coll}))))
      (interpose separator))
    coll))

(defn items [coll]
  (if (some coll? coll)
    (block :vertical (delimited-items coll))
    (block :horizontal (delimited-items coll))))

(defn sequential [xs]
  (block :vertical (delimited-items xs)))

(defn- emit-xf [rf]
  (fn
    ([] (rf))
    ([acc] (rf acc))
    ([acc input]
     (try
       ((stream input) rf acc)
       (catch Throwable ex
         ((as ex
            (raw-string
              (-> ex Throwable->map (assoc :phase :print-eval-result) m/ex-triage m/ex-str)
              {:fill ::style/error-color}))
          rf acc))))))

(defn- oneduce [xform f init x]
  (let [f (xform f)]
    (f (unreduced (f init x)))))

(defn- cell-xf
  [max-length]
  (comp
    emit-xf
    (map (fn [input]
           (case (:op input)
             ::push-block (assoc input :block :horizontal)
             ::separator {:op ::string :text " " :style {:selectable false}}
             input)))
    (fn [rf]
      (let [*pop-stack (volatile! [])
            *length (volatile! max-length)
            stash (ArrayList.)
            *string-index (volatile! -1)]
        (fn
          ([] (rf))
          ([acc]
           (if (neg? @*length)
             {:length max-length
              :steps (rf acc)}
             (let [stash-vec (vec (.toArray stash))]
               (.clear stash)
               {:length (- max-length @*length)
                :steps (reduce rf acc stash-vec)})))
          ([acc instruction]
           (.add stash instruction)
           (case (:op instruction)
             ::string
             (let [text (:text instruction)
                   text-len (count text)]
               (if (zero? text-len)
                 acc
                 (let [len @*length
                       new-len (- len text-len)]
                   (vreset! *length new-len)
                   (if (and (not (neg? len))
                            (neg? new-len))
                     (let [stash-vec (vec (.toArray stash))]
                       (.clear stash)
                       (ensure-reduced
                         (reduce
                           rf
                           acc
                           (-> stash-vec
                               (cond-> (pos? len) (update-in [(dec (count stash-vec)) :text] #(subs % 0 (dec len))))
                               (cond-> (zero? len)
                                       (-> pop
                                           (update-in [@*string-index :text] #(subs % 0 (dec (count %)))))) ;; what if length was 1? should remove it alltogether?
                               (conj {:op ::string :text "…" :style {:fill ::style/util-color}})
                               (into @*pop-stack)))))
                     (do (vreset! *string-index (dec (.size stash))) acc)))))
             ::push-block (do (vswap! *pop-stack conj {:op ::pop-block}) acc)
             ::pop-block (do (vswap! *pop-stack pop) acc)
             ::push-value (do (vswap! *pop-stack conj {:op ::pop-value}) acc)
             ::pop-value (do (vswap! *pop-stack pop) acc)
             acc)))))))

(defn- emit-ops [ops]
  (fn [rf acc]
    (reduce rf acc ops)))

(defn table [seqable]
  (let [xs (seq seqable)
        head (first xs)
        columns (cond
                  (map? head)
                  (for [k (keys head)]
                    {:header k :cell #(get % k)})

                  (map-entry? head)
                  [{:header 'key :cell key} {:header 'val :cell val}]

                  (indexed? head)
                  (for [i (range (count head))]
                    {:header i :cell #(nth % i)})

                  :else
                  [{:header 'value :cell identity}])
        ->cell (fn [x]
                 (assoc (oneduce (cell-xf 48) conj [] x) :value x))
        ->row (fn [x columns]
                (mapv #(->cell (try ((:cell %) x) (catch Throwable e e)))
                      columns))
        row-values (into [(mapv #(-> % :header ->cell) columns)]
                         (map #(->row % columns))
                         xs)
        cell-lengths (mapv :length (apply mapv #(apply max-key :length %&) row-values))]
    (block :vertical
      (streamduce
        (comp
          (map-indexed
            (fn stream-row [row cells]
              (block :horizontal
                (streamduce
                  (comp
                    (map-indexed
                      (fn stream-cell [col cell]
                        (let [pad (- (cell-lengths col) (:length cell))]
                          (if (and (pos? row) (number? (:value cell)))
                            (horizontal
                              (string (apply str (repeat pad \space)) {:selectable false})
                              (emit-ops (:steps cell)))
                            (horizontal
                              (emit-ops (:steps cell))
                              (string (apply str (repeat pad \space)) {:selectable false}))))))
                    (interpose (string " │ " {:fill ::style/unfocused-selection-color
                                              :selectable false})))
                  cells))))
          (interpose separator))
        row-values))))

(defn- blank-segment [n]
  {:text (apply str (repeat n \space))
   :width (* n (font/char-width \space))
   :style {:selectable false}})

(defn- string-segment [string-op]
  (-> string-op
      (dissoc :op)
      (assoc :width (transduce (map font/char-width) + (:text string-op)))))

(defn- segment-length [segment]
  (count (:text segment)))

(defn- segments-length [segments]
  (transduce (map segment-length) + segments))

(defn- next-index [region]
  (+ (:index region 0)
     (segments-length (:segments region))))

(defn- add-segment [line values segment]
  (let [last-region (peek line)]
    (if (not= (:values last-region) values)
      (conj line {:values values :segments [segment] :index (next-index last-region)})
      (update-in line [(dec (count line)) :segments] conj segment))))

(defn- add-separator [line]
  (let [last-region (peek line)]
    (cond-> line
            last-region
            (conj {:values (:values last-region)
                   :segments []
                   :index (next-index last-region)}))))

(defn- line-length [line]
  (next-index (peek line)))

(defn- format-xf [rf]
  (let [*state (volatile! {:line [] :values [] :blocks []})]
    (fn
      ([] (rf))
      ([acc] (rf acc))
      ([acc input]
       (let [state @*state]
         (case (:op input)
           ::push-value
           (do (vswap! *state update :values conj (:value input))
               acc)

           ::pop-value
           (do (vswap! *state update :values pop)
               acc)

           ::push-block
           (let [blocks (:blocks state)
                 block (peek blocks)]
             (case (:block block)
               :vertical
               (do (vswap! *state update :blocks conj {:block (:block input)
                                                       :indent (:indent block)})
                   acc)

               :horizontal
               (do (vswap! *state update :blocks conj {:block (:block input)
                                                       :indent (line-length (:line state))})
                   acc)

               nil
               (do (vswap! *state update :blocks conj {:block (:block input)
                                                       :indent 0})
                   acc)))

           ::pop-block
           (let [blocks (:blocks state)]
             (if (= 1 (count blocks))
               (do (vreset! *state (-> state
                                       (assoc :blocks (pop blocks))
                                       (assoc :line [])))
                   (rf acc (:line state)))
               (do (vreset! *state (assoc state :blocks (pop blocks)))
                   acc)))

           ::separator
           (let [blocks (:blocks state)
                 block (peek blocks)]
             (if (= :horizontal (:block block))
               (do (vswap! *state update :line #(-> %
                                                    add-separator
                                                    (add-segment (:values state) (blank-segment 1))
                                                    add-separator))
                   acc)
               (do (vswap! *state assoc :line (-> []
                                                  (add-segment (:values state) (blank-segment (:indent block 0)))
                                                  add-separator))
                   (rf acc (:line state)))))

           ::string
           (do (vswap! *state update :line add-segment (:values state) (string-segment input))
               acc)))))))

(def stream-xf
  (comp emit-xf format-xf))

(defn- identity-hash-code [x]
  (let [hash (System/identityHashCode x)]
    (as hash
      (raw-string (format "0x%x" hash) {:fill ::style/scalar-color}))))

(defmethod emit :default [x]
  (horizontal
    (raw-string "#object[" {:fill ::style/object-color})
    (let [c (class x)]
      (if (.isArray c)
        (as c (raw-string (pr-str (.getName c)) {:fill ::style/object-color}))
        (stream c)))
    separator
    (identity-hash-code x)
    separator
    (stream (str x))
    (raw-string "]" {:fill ::style/object-color})))

;; scalars

(defmethod emit nil [x]
  (raw-string (pr-str x) {:fill ::style/scalar-color}))

(defmethod emit Boolean [x]
  (raw-string (pr-str x) {:fill ::style/scalar-color}))

(defmethod emit String [s]
  (raw-string (pr-str s) {:fill ::style/string-color}))

(defmethod emit Character [ch]
  (raw-string (pr-str ch) {:fill ::style/string-color}))

(def escape-layout-chars
  {\newline "\\n"
   \tab "\\t"
   \return "\\r"
   \formfeed "\\f"
   \backspace "\\b"})

(defmethod emit Keyword [k]
  (escaped-string k {:fill ::style/keyword-color}
                  escape-layout-chars {:fill ::style/scalar-color}))

(defmethod emit Symbol [sym]
  (escaped-string sym {:fill ::style/symbol-color}
                  escape-layout-chars {:fill ::style/scalar-color}))

;; numbers

(defmethod emit Number [n]
  (raw-string n {:fill ::style/scalar-color}))

(defmethod emit Float [n]
  (raw-string
    (cond
      (= Float/POSITIVE_INFINITY n) "##Inf"
      (= Float/NEGATIVE_INFINITY n) "##-Inf"
      (Float/isNaN n) "##NaN"
      :else (str n))
    {:fill ::style/scalar-color}))

(defmethod emit Double [n]
  (raw-string
    (cond
      (= Double/POSITIVE_INFINITY n) "##Inf"
      (= Double/NEGATIVE_INFINITY n) "##-Inf"
      (Double/isNaN n) "##NaN"
      :else (str n))
    {:fill ::style/scalar-color}))

(defmethod emit BigDecimal [n]
  (raw-string (str n "M") {:fill ::style/scalar-color}))

(defmethod emit BigInt [n]
  (raw-string (str n "N") {:fill ::style/scalar-color}))

;; maps

(defmethod emit IPersistentMap [m]
  (horizontal
    (raw-string "{" {:fill ::style/object-color})
    (entries m)
    (raw-string "}" {:fill ::style/object-color})))

(defmethod emit IRecord [m]
  (horizontal
    (raw-string (str "#" (.getName (class m)) "{") {:fill ::style/object-color})
    (entries m)
    (raw-string "}" {:fill ::style/object-color})))

(defmethod emit Map [m]
  (horizontal
    (raw-string "{" {:fill ::style/object-color})
    (entries m)
    (raw-string "}" {:fill ::style/object-color})))

(prefer-method emit IRecord IPersistentMap)
(prefer-method emit IRecord Map)
(prefer-method emit IPersistentCollection Map)

;; lists

(defmethod emit IPersistentVector [v]
  (horizontal
    (raw-string "[" {:fill ::style/object-color})
    (items v)
    (raw-string "]" {:fill ::style/object-color})))

(defmethod emit List [xs]
  (horizontal
    (raw-string "(" {:fill ::style/object-color})
    (sequential xs)
    (raw-string ")" {:fill ::style/object-color})))

(defmethod emit RandomAccess [xs]
  (horizontal
    (raw-string "[" {:fill ::style/object-color})
    (items xs)
    (raw-string "]" {:fill ::style/object-color})))

(defmethod emit ISeq [xs]
  (horizontal
    (raw-string "(" {:fill ::style/object-color})
    (sequential xs)
    (raw-string ")" {:fill ::style/object-color})))

(prefer-method emit IPersistentCollection RandomAccess)
(prefer-method emit IPersistentCollection Collection)
(prefer-method emit RandomAccess List)
(prefer-method emit ISeq IPersistentCollection)
(prefer-method emit ISeq Collection)

;; sets

(defmethod emit IPersistentSet [s]
  (horizontal
    (raw-string "#{" {:fill ::style/object-color})
    (items s)
    (raw-string "}" {:fill ::style/object-color})))

(defmethod emit Set [s]
  (horizontal
    (raw-string "#{" {:fill ::style/object-color})
    (items s)
    (raw-string "}" {:fill ::style/object-color})))

;; exceptions

(defmethod emit Throwable [t]
  (horizontal
    (raw-string "#reveal/error" {:fill ::style/object-color})
    (stream (Throwable->map t))))

(defmethod emit StackTraceElement [^StackTraceElement el]
  (horizontal
    (raw-string "[" {:fill ::style/object-color})
    (stream (symbol (.getClassName el)))
    separator
    (stream (symbol (.getMethodName el)))
    separator
    (stream (.getFileName el))
    separator
    (stream (.getLineNumber el))
    (raw-string "]" {:fill ::style/object-color})))

;; objects

(def ^:private describe-multi
  (try
    (let [field (doto (.getDeclaredField MultiFn "name") (.setAccessible true))]
      #(symbol (.get field %)))
    (catch Exception _
      #(.-dispatchFn ^MultiFn %))))

(defmethod emit MultiFn [^MultiFn f]
  (horizontal
    (raw-string "#reveal/multi-fn[" {:fill ::style/object-color})
    (stream (describe-multi f))
    separator
    (identity-hash-code f)
    (raw-string "]" {:fill ::style/object-color})))

(defmethod emit Fn [f]
  (escaped-string (Compiler/demunge (.getName (class f))) {:fill ::style/object-color}
                  escape-layout-chars {:fill ::style/scalar-color}))

(defmethod emit Pattern [re]
  (horizontal
    (raw-string "#" {:fill ::style/object-color})
    (raw-string (str \" re \") {:fill ::style/string-color})))

(defmethod emit Var [var]
  (raw-string (pr-str var) {:fill ::style/object-color}))

(defmethod emit Namespace [^Namespace ns]
  (raw-string (name (.getName ns)) {:fill ::style/object-color}))

(defmethod emit Class [^Class class]
  (raw-string (.getName class) {:fill ::style/object-color}))

(defmethod emit Enum [^Enum enum]
  (raw-string
    (str (.getName (.getDeclaringClass enum)) "/" (.name enum))
    {:fill ::style/scalar-color}))

(defmethod emit IRef [*ref]
  (horizontal
    (raw-string (str "#reveal/" (.toLowerCase (.getSimpleName (class *ref))) "[") {:fill ::style/object-color})
    (stream @*ref)
    separator
    (identity-hash-code *ref)
    (raw-string "]" {:fill ::style/object-color})))

(defmethod emit File [file]
  (horizontal
    (raw-string "#reveal/file" {:fill ::style/object-color})
    separator
    (stream (str file))))

(defmethod emit Delay [*delay]
  (horizontal
    (raw-string "#reveal/delay[" {:fill ::style/object-color})
    (if (realized? *delay)
      (stream @*delay)
      (raw-string "..." {:fill ::style/util-color}))
    separator
    (identity-hash-code *delay)
    (raw-string "]" {:fill ::style/object-color})))

(defmethod emit Reduced [*reduced]
  (horizontal
    (raw-string "#reveal/reduced" {:fill ::style/object-color})
    separator
    (stream @*reduced)))

(defmethod emit IBlockingDeref [*blocking-deref]
  (let [class-name (.getName (class *blocking-deref))]
    (cond
      (.startsWith class-name "clojure.core$promise$reify")
      (horizontal
        (raw-string "#reveal/promise[" {:fill ::style/object-color})
        (if (realized? *blocking-deref)
          (stream @*blocking-deref)
          (raw-string "..." {:fill ::style/util-color}))
        separator
        (identity-hash-code *blocking-deref)
        (raw-string "]" {:fill ::style/object-color}))

      (.startsWith class-name "clojure.core$future_call$reify")
      (horizontal
        (raw-string "#reveal/future[" {:fill ::style/object-color})
        (if (realized? *blocking-deref)
          (stream @*blocking-deref)
          (raw-string "..." {:fill ::style/util-color}))
        separator
        (identity-hash-code *blocking-deref)
        (raw-string "]" {:fill ::style/object-color}))

      :else
      (raw-string (pr-str *blocking-deref) {:fill ::style/object-color}))))

(defmethod emit Volatile [*ref]
  (horizontal
    (raw-string "#reveal/volatile[" {:fill ::style/object-color})
    (stream @*ref)
    separator
    (identity-hash-code *ref)
    (raw-string "]" {:fill ::style/object-color})))

(defmethod emit TaggedLiteral [x]
  (horizontal
    (raw-string "#" {:fill ::style/object-color})
    (stream (:tag x))
    separator
    (stream (:form x))))

(defmethod emit ReaderConditional [^ReaderConditional x]
  (horizontal
    (raw-string (str "#?" (when (.-splicing x) "@")) {:fill ::style/object-color})
    (stream (.-form x))))

(defmethod emit URL [x]
  (horizontal
    (raw-string "#reveal/url" {:fill ::style/object-color})
    separator
    (stream (str x))))

(defmethod emit URI [x]
  (horizontal
    (raw-string "#reveal/uri" {:fill ::style/object-color})
    separator
    (stream (str x))))

(defmethod emit UUID [x]
  (horizontal
    (raw-string "#uuid" {:fill ::style/object-color})
    separator
    (stream (str x))))

(defn system-out [line]
  (as line
    (raw-string line {:fill ::style/string-color})))

(defn system-err [line]
  (as line
    (raw-string line {:fill ::style/error-color})))

(defmethod emit Eduction [eduction]
  (horizontal
    (raw-string "(" {:fill ::style/object-color})
    (sequential eduction)
    (raw-string ")" {:fill ::style/object-color})))

(def ^:private ^ThreadLocal utc-date-format
  (proxy [ThreadLocal] []
    (initialValue []
      (doto (SimpleDateFormat. "yyyy-MM-dd'T'HH:mm:ss.SSS-00:00")
        (.setTimeZone (TimeZone/getTimeZone "GMT"))))))

(defmethod emit Date [^Date date]
  (let [^DateFormat format (.get utc-date-format)]
    (horizontal
      (raw-string "#inst" {:fill ::style/object-color})
      separator
      (stream (.format format date)))))

(defmethod emit Calendar [^Calendar calendar]
  (let [calendar-str (format "%1$tFT%1$tT.%1$tL%1$tz" calendar)
        minutes-index (- (.length calendar-str) 2)]
    (horizontal
      (raw-string "#inst" {:fill ::style/object-color})
      separator
      (stream (str (subs calendar-str 0 minutes-index) ":" (subs calendar-str minutes-index))))))

(defmethod emit Instant [instant]
  (horizontal
    (raw-string "#inst" {:fill ::style/object-color})
    separator
    (stream (str instant))))

(defmacro ^:private when-class [class-name & body]
  `(try
     (Class/forName ^String ~class-name)
     ~@body
     (catch ClassNotFoundException _#)))

(when-class "java.sql.Timestamp"
  (def ^:private utc-timestamp-format
    (proxy [ThreadLocal] []
      (initialValue []
        (doto (SimpleDateFormat. "yyyy-MM-dd'T'HH:mm:ss")
          (.setTimeZone (TimeZone/getTimeZone "GMT"))))))

  (defmethod emit java.sql.Timestamp [^java.sql.Timestamp timestamp]
    (horizontal
      (raw-string "#inst" {:fill ::style/object-color})
      separator
      (stream (str (.format ^DateFormat (.get ^ThreadLocal utc-timestamp-format) timestamp)
                   (format ".%09d-00:00" (.getNanos timestamp)))))))