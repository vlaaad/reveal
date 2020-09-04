(ns vlaaad.reveal.stream
  (:refer-clojure :exclude [newline])
  (:require [vlaaad.reveal.style :as style]
            [clojure.main :as m])
  (:import [clojure.lang Keyword Symbol IPersistentMap IPersistentVector IPersistentSet Fn
                         ISeq MultiFn IRef Var Volatile Namespace IRecord Delay
                         IBlockingDeref TaggedLiteral Reduced ReaderConditional
                         IPersistentCollection BigInt]
           [java.util.regex Pattern]
           [java.io File FileNotFoundException]
           [java.net URL URI]
           [java.util UUID List Collection RandomAccess Map Set TimeZone Date Calendar]
           [clojure.core Eduction]
           [java.text SimpleDateFormat DateFormat]
           [java.time Instant]))

(set! *warn-on-reflection* true)

(defrecord AnnotatedValue [value annotation])

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

(defn with-value [x ann sf]
  (=> (op {:op ::push-value :value (->AnnotatedValue x ann)})
      sf
      (op {:op ::pop-value})))

(def separator
  (op {:op ::separator}))

(def ^:private newline
  (op {:op ::newline}))

(defn- string [str style]
  (op {:op ::string
       :text str
       :style style}))

(defn- block [block-type sf]
  (=> (op {:op ::push-block :block block-type})
      sf
      (op {:op ::pop-block})))

;; endregion
(defmulti stream-dispatch (fn stream-dispatch [x _]
                            (or (::type (meta x)) (class x))))

(defn stream
  ([x] (stream-dispatch x nil))
  ([x ann] (stream-dispatch x ann)))

(defn as [x sf]
  (with-value x {::hidden true} sf))

(defmethod stream-dispatch ::just [sf _] sf)

(defn as-is [sf]
  (with-meta sf {::type ::just}))

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
      (string str {:fill style/util-color})))

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
                \newline ((=> (flush-builder builder style) newline) rf acc)
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
                    \newline ((=> (flush-builder builder style) newline) rf acc)
                    \tab (do (.append builder "    ") acc)
                    ;; todo except if followed by newline...
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
  (block :vertical (apply => (interpose newline sfs))))

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
                 (horizontal (stream k {:vlaaad.reveal.nav/val v
                                        :vlaaad.reveal.nav/coll m})
                             separator
                             (stream v {:vlaaad.reveal.nav/key k
                                        :vlaaad.reveal.nav/coll m})))))
        (interpose newline))
      m)))

(defn- delimited-items [coll sep]
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
      (interpose sep))
    coll))

(defn vertically [xs]
  (block :vertical (delimited-items xs newline)))

(defn horizontally [xs]
  (block :horizontal (delimited-items xs separator)))

(defn items [coll]
  (if (some coll? coll)
    (vertically coll)
    (horizontally coll)))

(defn override-style [sf f & args]
  (fn [rf acc]
    (let [rf (fn
               ([acc] (rf acc))
               ([acc input]
                (rf acc (case (:op input)
                          ::string (apply update input :style f args)
                          input))))]
      (sf rf acc))))

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
              {:fill style/error-color}))
          rf acc))))))

(defn- oneduce [xform f init x]
  (let [f (xform f)]
    (f (unreduced (f init x)))))

(defn fx-summary [max-length value]
  (oneduce
    (comp
      emit-xf
      (keep #(case (:op %)
               ::string {:fx/type :text
                         :text (:text %)
                         :fill (:fill (:style %) :black)}
               (::separator ::newline) {:fx/type :text
                                         :text " "
                                         :fill style/util-color}
               nil)))
    (fn
      ([acc] {:fx/type :text-flow
              :style-class "reveal-summary"
              :children (:children acc)})
      ([{:keys [length children] :as acc} desc]
       (let [text (:text desc)
             text-length (count text)
             new-length (- length text-length)]
         (cond
           (or (zero? text-length) (neg? length))
           acc

           (and
             (neg? new-length)
             (not (neg? length)))
           (reduced
             {:children (-> (if (pos? length)
                              (conj children
                                    (assoc desc :text (subs text 0 (dec length))))
                              (update-in children [(dec (count children)) :text]
                                         #(subs % 0 (dec (count %)))))
                            (conj {:fx/type :text
                                   :text "…"
                                   :fill style/util-color}))
              :length new-length})

           :else
           {:children (conj children desc)
            :length new-length}))))
    {:length max-length
     :children []}
    value))

(defn str-summary
  ([value]
   (str-summary 48 value))
  ([max-length value]
   (oneduce
     (comp
       emit-xf
       (keep #(case (:op %)
                ::string (:text %)
                (::separator ::newline) " "
                nil)))
     (fn
       ([{:keys [^StringBuilder builder]}]
        (.toString builder))
       ([{:keys [length ^StringBuilder builder] :as acc} ^String text]
        (let [text-length (count text)
              new-length (- length text-length)]
          (cond
            (or (zero? text-length) (neg? length))
            acc

            (and
              (neg? new-length)
              (not (neg? length)))
            (reduced
              {:builder (doto builder
                          (as-> $
                                (if (pos? length)
                                  (.append $ (subs text 0 (dec length)))
                                  (.delete $ ^int (dec max-length) ^int max-length)))
                          (.append "…"))
               :length new-length})

            :else
            {:builder (doto builder (.append text))
             :length new-length}))))
     {:length max-length
      :builder (StringBuilder.)}
     value)))

(defn ->str [value]
  (oneduce
    (comp
      emit-xf
      (keep #(case (:op %)
               ::string (:text %)
               (::separator ::newline) " "
               nil)))
    (fn
      ([^StringBuilder builder]
       (.toString builder))
      ([^StringBuilder builder ^String text]
       (doto builder (.append text))))
    (StringBuilder.)
    value))

(defn- blank-segment [n]
  {:text (apply str (repeat n \space))
   :style {:selectable false}})

(defn- string-segment [string-op]
  (dissoc string-op :op))

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

           ::newline
           (let [blocks (:blocks state)
                 block (peek blocks)]
             (do (vswap! *state assoc :line (-> []
                                                (add-segment (:values state) (blank-segment (:indent block 0)))
                                                add-separator))
                 (rf acc (:line state))))

           ::separator
           (let [blocks (:blocks state)
                 block (peek blocks)]
             (if (= :horizontal (:block block))
               (do (vswap! *state update :line #(-> %
                                                    add-separator
                                                    (add-segment (:values state) (blank-segment 1))
                                                    add-separator))
                   acc)
               (do (vswap! *state update :line add-segment (:values state) (blank-segment 0))
                   acc)))

           ::string
           (do (vswap! *state update :line add-segment (:values state) (string-segment input))
               acc)))))))

(def stream-xf
  (comp emit-xf format-xf))

(defn- identity-hash-code [x]
  (let [hash (System/identityHashCode x)]
    (as hash
      (raw-string (format "0x%x" hash) {:fill style/scalar-color}))))

(defmacro defstream [dispatch-val bindings sf]
  (let [[x ann] (cond-> bindings (= 1 (count bindings)) (conj (gensym "ann")))]
    `(defmethod stream-dispatch ~dispatch-val [x# ann#]
       (let [~x x#
             ~ann ann#]
         (with-value x# ann# ~sf)))))

(defstream :default [x]
  (horizontal
    (raw-string "#object[" {:fill style/object-color})
    (let [c (class x)]
      (if (.isArray c)
        (as c (raw-string (pr-str (.getName c)) {:fill style/object-color}))
        (stream c)))
    separator
    (identity-hash-code x)
    separator
    (stream (str x))
    (raw-string "]" {:fill style/object-color})))

;; scalars

(defstream nil [x]
  (raw-string (pr-str x) {:fill style/scalar-color}))

(defstream Boolean [x]
  (raw-string (pr-str x) {:fill style/scalar-color}))

(defstream String [s]
  (raw-string (pr-str s) {:fill style/string-color}))

(defstream Character [ch]
  (raw-string (pr-str ch) {:fill style/string-color}))

(def escape-layout-chars
  {\newline "\\n"
   \tab "\\t"
   \return "\\r"
   \formfeed "\\f"
   \backspace "\\b"})

(defstream Keyword [k]
  (escaped-string k {:fill style/keyword-color}
                  escape-layout-chars {:fill style/scalar-color}))

(defstream Symbol [sym]
  (escaped-string sym {:fill style/symbol-color}
                  escape-layout-chars {:fill style/scalar-color}))

;; numbers

(defstream Number [n]
  (raw-string n {:fill style/scalar-color}))

(defstream Float [n]
  (raw-string
    (cond
      (= Float/POSITIVE_INFINITY n) "##Inf"
      (= Float/NEGATIVE_INFINITY n) "##-Inf"
      (Float/isNaN n) "##NaN"
      :else (str n))
    {:fill style/scalar-color}))

(defstream Double [n]
  (raw-string
    (cond
      (= Double/POSITIVE_INFINITY n) "##Inf"
      (= Double/NEGATIVE_INFINITY n) "##-Inf"
      (Double/isNaN n) "##NaN"
      :else (str n))
    {:fill style/scalar-color}))

(defstream BigDecimal [n]
  (raw-string (str n "M") {:fill style/scalar-color}))

(defstream BigInt [n]
  (raw-string (str n "N") {:fill style/scalar-color}))

;; maps

(defstream IPersistentMap [m]
  (horizontal
    (raw-string "{" {:fill style/object-color})
    (entries m)
    (raw-string "}" {:fill style/object-color})))

(defstream IRecord [m]
  (horizontal
    (raw-string (str "#" (.getName (class m)) "{") {:fill style/object-color})
    (entries m)
    (raw-string "}" {:fill style/object-color})))

(defstream Map [m]
  (horizontal
    (raw-string "{" {:fill style/object-color})
    (entries m)
    (raw-string "}" {:fill style/object-color})))

(prefer-method stream-dispatch IRecord IPersistentMap)
(prefer-method stream-dispatch IRecord Map)
(prefer-method stream-dispatch IPersistentCollection Map)

;; lists

(defstream IPersistentVector [v]
  (horizontal
    (raw-string "[" {:fill style/object-color})
    (items v)
    (raw-string "]" {:fill style/object-color})))

(defstream List [xs]
  (horizontal
    (raw-string "(" {:fill style/object-color})
    (vertically xs)
    (raw-string ")" {:fill style/object-color})))

(defstream RandomAccess [xs]
  (horizontal
    (raw-string "[" {:fill style/object-color})
    (items xs)
    (raw-string "]" {:fill style/object-color})))

(defstream ISeq [xs]
  (horizontal
    (raw-string "(" {:fill style/object-color})
    (vertically xs)
    (raw-string ")" {:fill style/object-color})))

(prefer-method stream-dispatch IPersistentCollection RandomAccess)
(prefer-method stream-dispatch IPersistentCollection Collection)
(prefer-method stream-dispatch RandomAccess List)
(prefer-method stream-dispatch ISeq IPersistentCollection)
(prefer-method stream-dispatch ISeq Collection)

;; sets

(defstream IPersistentSet [s]
  (horizontal
    (raw-string "#{" {:fill style/object-color})
    (items s)
    (raw-string "}" {:fill style/object-color})))

(defstream Set [s]
  (horizontal
    (raw-string "#{" {:fill style/object-color})
    (items s)
    (raw-string "}" {:fill style/object-color})))

;; exceptions

(defstream Throwable [t]
  (horizontal
    (raw-string "#reveal/error" {:fill style/object-color})
    (stream (Throwable->map t))))

(defstream StackTraceElement [^StackTraceElement el]
  (horizontal
    (raw-string "[" {:fill style/object-color})
    (stream (symbol (.getClassName el)))
    separator
    (stream (symbol (.getMethodName el)))
    separator
    (stream (.getFileName el))
    separator
    (stream (.getLineNumber el))
    (raw-string "]" {:fill style/object-color})))

;; objects

(def ^:private describe-multi
  (try
    (let [field (doto (.getDeclaredField MultiFn "name") (.setAccessible true))]
      #(symbol (.get field %)))
    (catch Exception _
      #(.-dispatchFn ^MultiFn %))))

(defstream MultiFn [^MultiFn f]
  (horizontal
    (raw-string "#reveal/multi-fn[" {:fill style/object-color})
    (stream (describe-multi f))
    separator
    (identity-hash-code f)
    (raw-string "]" {:fill style/object-color})))

(defstream Fn [f]
  (escaped-string (Compiler/demunge (.getName (class f))) {:fill style/object-color}
                  escape-layout-chars {:fill style/scalar-color}))

(defstream Pattern [re]
  (horizontal
    (raw-string "#" {:fill style/object-color})
    (raw-string (str \" re \") {:fill style/string-color})))

(defstream Var [var]
  (raw-string (pr-str var) {:fill style/object-color}))

(defstream Namespace [^Namespace ns]
  (raw-string (name (.getName ns)) {:fill style/object-color}))

(defstream Class [^Class class]
  (raw-string (.getName class) {:fill style/object-color}))

(defstream Enum [^Enum enum]
  (raw-string
    (str (.getName (.getDeclaringClass enum)) "/" (.name enum))
    {:fill style/scalar-color}))

(defstream IRef [*ref]
  (horizontal
    (raw-string (str "#reveal/" (.toLowerCase (.getSimpleName (class *ref))) "[") {:fill style/object-color})
    (stream @*ref)
    separator
    (identity-hash-code *ref)
    (raw-string "]" {:fill style/object-color})))

(defstream File [file]
  (horizontal
    (raw-string "#reveal/file" {:fill style/object-color})
    separator
    (stream (str file))))

(defstream Delay [*delay]
  (horizontal
    (raw-string "#reveal/delay[" {:fill style/object-color})
    (if (realized? *delay)
      (stream @*delay)
      (raw-string "..." {:fill style/util-color}))
    separator
    (identity-hash-code *delay)
    (raw-string "]" {:fill style/object-color})))

(defstream Reduced [*reduced]
  (horizontal
    (raw-string "#reveal/reduced" {:fill style/object-color})
    separator
    (stream @*reduced)))

(defstream IBlockingDeref [*blocking-deref]
  (let [class-name (.getName (class *blocking-deref))]
    (cond
      (.startsWith class-name "clojure.core$promise$reify")
      (horizontal
        (raw-string "#reveal/promise[" {:fill style/object-color})
        (if (realized? *blocking-deref)
          (stream @*blocking-deref)
          (raw-string "..." {:fill style/util-color}))
        separator
        (identity-hash-code *blocking-deref)
        (raw-string "]" {:fill style/object-color}))

      (.startsWith class-name "clojure.core$future_call$reify")
      (horizontal
        (raw-string "#reveal/future[" {:fill style/object-color})
        (if (realized? *blocking-deref)
          (stream @*blocking-deref)
          (raw-string "..." {:fill style/util-color}))
        separator
        (identity-hash-code *blocking-deref)
        (raw-string "]" {:fill style/object-color}))

      :else
      (raw-string (pr-str *blocking-deref) {:fill style/object-color}))))

(defstream Volatile [*ref]
  (horizontal
    (raw-string "#reveal/volatile[" {:fill style/object-color})
    (stream @*ref)
    separator
    (identity-hash-code *ref)
    (raw-string "]" {:fill style/object-color})))

(defstream TaggedLiteral [x]
  (horizontal
    (raw-string "#" {:fill style/object-color})
    (stream (:tag x))
    separator
    (stream (:form x))))

(defstream ReaderConditional [^ReaderConditional x]
  (horizontal
    (raw-string (str "#?" (when (.-splicing x) "@")) {:fill style/object-color})
    (stream (.-form x))))

(defstream URL [x]
  (horizontal
    (raw-string "#reveal/url" {:fill style/object-color})
    separator
    (stream (str x))))

(defstream URI [x]
  (horizontal
    (raw-string "#reveal/uri" {:fill style/object-color})
    separator
    (stream (str x))))

(defstream UUID [x]
  (horizontal
    (raw-string "#uuid" {:fill style/object-color})
    separator
    (stream (str x))))

(defstream Eduction [eduction]
  (horizontal
    (raw-string "(" {:fill style/object-color})
    (vertically eduction)
    (raw-string ")" {:fill style/object-color})))

(def ^:private ^ThreadLocal utc-date-format
  (proxy [ThreadLocal] []
    (initialValue []
      (doto (SimpleDateFormat. "yyyy-MM-dd'T'HH:mm:ss.SSS-00:00")
        (.setTimeZone (TimeZone/getTimeZone "GMT"))))))

(defstream Date [^Date date]
  (let [^DateFormat format (.get utc-date-format)]
    (horizontal
      (raw-string "#inst" {:fill style/object-color})
      separator
      (stream (.format format date)))))

(defstream Calendar [^Calendar calendar]
  (let [calendar-str (format "%1$tFT%1$tT.%1$tL%1$tz" calendar)
        minutes-index (- (.length calendar-str) 2)]
    (horizontal
      (raw-string "#inst" {:fill style/object-color})
      separator
      (stream (str (subs calendar-str 0 minutes-index) ":" (subs calendar-str minutes-index))))))

(defstream Instant [instant]
  (horizontal
    (raw-string "#inst" {:fill style/object-color})
    separator
    (stream (str instant))))

(defmacro ^:private when-class [class-name & body]
  `(try
     (Class/forName ^String ~class-name)
     ~@body
     (catch ClassNotFoundException _#)))

(when-class "java.sql.Timestamp"
  (load "stream/sql_timestamp"))

(defmacro ^:private when-ns [ns-sym & body]
  `(try
     (require '~ns-sym)
     ~@body
     (catch FileNotFoundException _#)))

(when-ns lambdaisland.deep-diff.diff
  (load "stream/deep_diff"))

(when-ns lambdaisland.deep-diff2.diff-impl
  (load "stream/deep_diff2"))