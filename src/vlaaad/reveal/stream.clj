(ns vlaaad.reveal.stream
  (:refer-clojure :exclude [newline])
  (:require [cljfx.fx.text :as fx.text]
            [cljfx.fx.text-flow :as fx.text-flow]
            [clojure.main :as m]
            [lambdaisland.deep-diff2.diff-impl]
            [vlaaad.reveal.ns :as ns]
            [vlaaad.reveal.prefs :as prefs]
            [vlaaad.reveal.style :as style])
  (:import [clojure.core Eduction]
           [clojure.lang BigInt Delay Fn IBlockingDeref IPersistentCollection IPersistentMap
                         IPersistentSet IPersistentVector IRecord IRef ISeq Keyword MultiFn Namespace
                         PersistentQueue ReaderConditional Reduced Symbol
                         TaggedLiteral Var Volatile]
           [java.io File]
           [java.lang.reflect Constructor Field Method Proxy]
           [java.net URI URL]
           [java.text DateFormat SimpleDateFormat]
           [java.time Instant]
           [java.util Calendar Collection Date List Map RandomAccess Set TimeZone UUID]
           [java.util.concurrent Future]
           [java.util.regex Pattern]
           [lambdaisland.deep_diff2.diff_impl Deletion Insertion Mismatch]))

(set! *warn-on-reflection* true)

(defrecord AnnotatedValue [value annotation])

(def ^{:private true :tag 'long} slim-key-character-limit 6) ; Fits quotes + four chars.
(def ^{:private true :tag 'long} slim-value-character-limit 20) ; Fits long integer.
(def ^{:private true :tag 'long} slim-type-tag-character-limit slim-value-character-limit)

(def ^:private *slim-format (delay (= :slim (:formatting @prefs/prefs :default))))

(defn as-is [sf]
  (with-meta sf {::type ::as-is}))

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
  (as-is (=> (op {:op ::push-value :value (->AnnotatedValue x ann)})
             sf
             (op {:op ::pop-value}))))

(def separator
  (as-is (op {:op ::separator})))

(def ^:private newline
  (op {:op ::newline}))

(def newrow
  (op {:op ::newrow}))

(defn- string [str style]
  (op {:op ::string
       :text str
       :style style}))

(defn- block [block-type sf]
  (as-is (=> (op {:op ::push-block :block block-type})
             sf
             (op {:op ::pop-block}))))

;; endregion
(defmulti stream-dispatch (fn stream-dispatch [x _]
                            (or (::type (meta x)) (class x))))

(defn stream
  ([x] (stream-dispatch x nil))
  ([x ann] (stream-dispatch x ann)))

(defn as
  ([x sf]
   (as x nil sf))
  ([x ann sf]
   (with-value x (assoc ann ::hidden true) sf)))

(defmethod stream-dispatch ::as-is [sf _] sf)

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
      (string str {:fill :util})))

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
                \return (cond
                          (== i (dec len)) ((flush+util builder style "\\r") rf acc)
                          (= \newline (.charAt str (inc i))) acc
                          :else ((flush+util builder style "\\r") rf acc))
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
                    \return (cond
                              (== i (dec len)) ((flush+util builder style "\\r") rf acc)
                              (= \newline (.charAt str (inc i))) acc
                              :else ((flush+util builder style "\\r") rf acc))
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
  (block :vertical (apply => (interpose (=> newline newrow) sfs))))

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

(defn- preserving-reduced [rf]
  #(let [ret (rf %1 %2)]
     (if (reduced? ret)
       (reduced ret)
       ret)))

(defn- streamduce [xf coll]
  (fn [rf acc]
    (let [rrf (preserving-reduced rf)]
      (transduce
        xf
        (fn
          ([acc] acc)
          ([acc sf] (sf rrf acc)))
        acc
        coll))))

(defn- sf-wider-than? [sf ^long max-width]
  (->> 0
       (sf (fn [^long width input]
             (let [width (case (:op input)
                           ::separator (inc width)
                           ::string (+ width (count (:text input)))
                           ::newline 0
                           width)]
               (if (< max-width width)
                 (reduced width)
                 width))))
       (unreduced)
       (long)
       (< max-width)))

(defn- sf-multi-line? [sf]
  (unreduced
    (sf
      #(case (:op %2)
         ::newline (reduced true)
         false)
      nil)))

(defn entries
  ([m]
   (entries m nil))
  ([m ann]
   (block :vertical
     (streamduce
       (comp
         (map (fn [e]
                (let [k (key e)
                      v (val e)
                      ksf (stream k (assoc ann :vlaaad.reveal.nav/val v
                                               :vlaaad.reveal.nav/coll m))
                      vsf (stream v (assoc ann :vlaaad.reveal.nav/key k
                                               :vlaaad.reveal.nav/coll m))]
                  (if (and @*slim-format
                           (sf-multi-line? vsf)
                           (sf-wider-than? ksf slim-key-character-limit))
                    (vertical ksf
                              (horizontal separator
                                          separator
                                          vsf))
                    (horizontal ksf
                                separator
                                vsf)))))
         (interpose (=> newline newrow)))
       m))))

(defn- delimited-items [coll sep ann]
  (streamduce
    (comp
      (if (set? coll)
        (map
          (fn [x]
            (stream x (assoc ann :vlaaad.reveal.nav/key x
                                 :vlaaad.reveal.nav/coll coll))))
        (map-indexed
          (fn [i x]
            (stream x (assoc ann :vlaaad.reveal.nav/key i
                                 :vlaaad.reveal.nav/coll coll)))))
      (interpose sep))
    coll))

(defn vertically
  ([xs] (vertically xs nil))
  ([xs ann] (block :vertical (delimited-items xs (=> newline newrow) ann))))

(defn horizontally
  ([xs] (horizontally xs nil))
  ([xs ann] (block :horizontal (delimited-items xs separator ann))))

(defn horizontal-item? [x]
  (cond
    (coll? x) false
    (nil? x) true
    (boolean? x) true
    (number? x) true
    (char? x) true
    (or (string? x)
        (keyword? x)
        (symbol? x)) (not (sf-wider-than? (stream x) slim-value-character-limit))
    :else false))

(defn horizontal-coll? [coll]
  (if @*slim-format
    (every? horizontal-item? coll)
    (not-any? coll? coll)))

(defn items
  ([coll]
   (items coll nil))
  ([coll ann]
   (if (horizontal-coll? coll)
     (horizontally coll ann)
     (vertically coll ann))))

(defn override-style [sf f & args]
  (as-is (fn [rf acc]
           (let [rf (fn
                      ([acc] (rf acc))
                      ([acc input]
                       (rf acc (case (:op input)
                                 ::string (apply update input :style f args)
                                 input))))]
             (sf rf acc)))))

(defn single-line [sf]
  (let [space-op (string " " {})]
    (as-is
      (fn [rf acc]
        (let [rf (fn
                   ([acc] (rf acc))
                   ([acc input]
                    (case (:op input)
                      ::newrow acc
                      (::separator ::newline) (space-op rf acc)
                      (rf acc input))))]
          (sf rf acc))))))

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
              {:fill :error}))
          rf acc))))))

(defn- oneduce [xform f init x]
  (let [f (xform f)]
    (f (unreduced (f init x)))))

(defn fx-summary [max-length value]
  (oneduce
    (comp
      emit-xf
      (keep #(case (:op %)
               ::string {:fx/type fx.text/lifecycle
                         :text (:text %)
                         :fill (style/color (:fill (:style %) :black))}
               (::separator ::newline) {:fx/type fx.text/lifecycle
                                        :text " "
                                        :fill (style/color :util)}
               nil)))
    (fn
      ([acc] {:fx/type fx.text-flow/lifecycle
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
                            (conj {:fx/type fx.text/lifecycle
                                   :text "…"
                                   :fill (style/color :util)}))
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
  (let [sb (StringBuilder.)]
    (dotimes [_ n] (.append sb \space))
    {:text (.toString sb)}))

(defn- string-segment [string-op]
  (dissoc string-op :op))

(defn- segment-length [segment]
  (count (:text segment)))

(defn- segments-length [segments]
  (transduce (map segment-length) + segments))

(defn- next-index [region]
  (+ (:index region 0)
     (segments-length (:segments region))))

(defn- set-last [xs x]
  (assoc xs (dec (count xs)) x))

(defn- add-non-selectable-segment [line segment]
  (let [last-region (peek line)]
    (if (:selectable last-region true)
      (conj line {:selectable false :segments [segment] :index (next-index last-region)})
      (update-in line [(dec (count line)) :segments] conj segment))))

(defn- add-selectable-segment [line value ids *row-starts segment]
  (let [last-region (peek line)]
    (if (or (not (identical? (:value last-region) value))
            (not (:selectable last-region false)))
      (let [row-starts @*row-starts
            row-start (and (some? value)
                           (peek row-starts))]
        (when row-start
          (vswap! *row-starts set-last false))
        (conj line {:value value
                    :selectable true
                    :segments [segment]
                    :index (next-index last-region)
                    :nav {:ids ids
                          :start-row row-start}}))
      (update-in line [(dec (count line)) :segments] conj segment))))

;; start-row?..

(defn- line-length [line]
  (next-index (peek line)))

(defonce ^:private *id (atom 0))

(defn- next-id [] (swap! *id inc))

(defn- format-xf [rf]
  (let [*values (volatile! [])
        *ids (volatile! [])
        *row-starts (volatile! [true])
        *line (volatile! [])
        *blocks (volatile! [])]
    (fn
      ([] (rf))
      ([acc] (rf acc))
      ([acc input]
       (let [line @*line]
         (case (:op input)
           ::push-value
           (do (vswap! *values conj (:value input))
               (vswap! *ids conj (next-id))
               acc)

           ::pop-value
           (do (vswap! *values pop)
               (vswap! *ids pop)
               acc)

           ::push-block
           (let [block (:block input)
                 parent (peek @*blocks)]
             (when (= :vertical block)
               (vswap! *row-starts conj true))
             (case (:block parent)
               :vertical (vswap! *blocks conj {:block block :indent (:indent parent)})
               :horizontal (vswap! *blocks conj {:block block :indent (line-length line)})
               nil (vswap! *blocks conj {:block block :indent 0}))
             acc)

           ::pop-block
           (let [blocks @*blocks]
             (when (= :vertical (:block (peek blocks)))
               (vswap! *row-starts pop))
             (if (= 1 (count blocks))
               (do (vreset! *blocks (pop blocks))
                   (vreset! *line [])
                   (rf acc line))
               (do (vswap! *blocks pop) acc)))

           ::newline
           (do (vreset! *line (add-non-selectable-segment [] (blank-segment (:indent (peek @*blocks) 0))))
               (rf acc line))

           ::newrow
           (do (vswap! *row-starts set-last true) acc)

           ::separator
           (if (= :horizontal (:block (peek @*blocks)))
             (do (vswap! *line add-non-selectable-segment (blank-segment 1)) acc)
             acc)

           ::string
           (do (if (:selectable (:style input) true)
                 (vswap! *line add-selectable-segment (peek @*values) @*ids *row-starts (string-segment input))
                 (vswap! *line add-non-selectable-segment (string-segment input)))
               acc)))))))

(def stream-xf
  (comp emit-xf format-xf))

(defn- identity-hash-code [x]
  (let [hash (System/identityHashCode x)]
    (as hash
      (raw-string (format "0x%x" hash) {:fill :scalar}))))

(defn- identity-hash-code-comment [x]
  (let [hash (System/identityHashCode x)]
    (as hash (raw-string (format "#_0x%x" hash) {:fill :util}))))

(defn type-tagged
  ([class-or-symbol value]
   (type-tagged class-or-symbol {:fill :object} value))
  ([class-or-symbol type-tag-style value]
   (let [type-name (cond
                     (class? class-or-symbol)
                     (.getName ^Class class-or-symbol)

                     (symbol? class-or-symbol)
                     (str class-or-symbol)

                     :else
                     (throw (IllegalArgumentException. "class-or-symbol must be a class or a symbol.")))
         type-tag-sf (raw-string (str "#" type-name) type-tag-style)
         value-sf (stream value)
         break (and @*slim-format
                    (or (sf-multi-line? value-sf)
                        (sf-wider-than? type-tag-sf slim-type-tag-character-limit)))]
     (if break
       (vertical type-tag-sf value-sf)
       (horizontal type-tag-sf separator value-sf)))))

(defmacro defstream [dispatch-val bindings sf]
  (let [[x ann] (cond-> bindings (= 1 (count bindings)) (conj (gensym "ann")))]
    `(defmethod stream-dispatch ~dispatch-val [x# ann#]
       (let [~x x#
             ~ann ann#]
         (with-value x# ann# ~sf)))))

(defstream :default [x]
  (horizontal
    (raw-string "#object[" {:fill :object})
    (let [c (class x)]
      (if (.isArray c)
        (as c (raw-string (pr-str (.getName c)) {:fill :object}))
        (stream c)))
    separator
    (identity-hash-code x)
    separator
    (stream (str x))
    (raw-string "]" {:fill :object})))

;; scalars

(defstream nil [x]
  (raw-string (pr-str x) {:fill :scalar}))

(defstream Boolean [x]
  (raw-string (pr-str x) {:fill (if (or (identical? Boolean/TRUE x) (identical? Boolean/FALSE x)) :scalar :error)}))

(defstream String [s]
  (raw-string (pr-str s) {:fill :string}))

(defstream Character [ch]
  (raw-string (pr-str ch) {:fill :string}))

(def escape-layout-chars
  {\newline "\\n"
   \tab "\\t"
   \return "\\r"
   \formfeed "\\f"
   \backspace "\\b"})

(defstream Keyword [k]
  (escaped-string k {:fill :keyword}
                  escape-layout-chars {:fill :scalar}))

(defstream Symbol [sym]
  (escaped-string sym {:fill :symbol}
                  escape-layout-chars {:fill :scalar}))

;; numbers

(defstream Number [n]
  (raw-string n {:fill :scalar}))

(defstream Float [n]
  (raw-string
    (cond
      (= Float/POSITIVE_INFINITY n) "##Inf"
      (= Float/NEGATIVE_INFINITY n) "##-Inf"
      (Float/isNaN n) "##NaN"
      :else (str n))
    {:fill :scalar}))

(defstream Double [n]
  (raw-string
    (cond
      (= Double/POSITIVE_INFINITY n) "##Inf"
      (= Double/NEGATIVE_INFINITY n) "##-Inf"
      (Double/isNaN n) "##NaN"
      :else (str n))
    {:fill :scalar}))

(defstream BigDecimal [n]
  (raw-string (str n "M") {:fill :scalar}))

(defstream BigInt [n]
  (raw-string (str n "N") {:fill :scalar}))

;; maps

(defstream IPersistentMap [m]
  (horizontal
    (raw-string "{" {:fill :object})
    (entries m)
    (raw-string "}" {:fill :object})))

(defstream IRecord [m]
  (type-tagged
    (class m) {:fill :object}
    (horizontal
      (raw-string "{" {:fill :object})
      (entries m)
      (raw-string "}" {:fill :object}))))

(defstream Map [m]
  (horizontal
    (raw-string "{" {:fill :object})
    (entries m)
    (raw-string "}" {:fill :object})))

(prefer-method stream-dispatch IRecord IPersistentMap)
(prefer-method stream-dispatch IRecord Map)
(prefer-method stream-dispatch IPersistentCollection Map)

;; lists

(defstream IPersistentVector [v]
  (horizontal
    (raw-string "[" {:fill :object})
    (items v)
    (raw-string "]" {:fill :object})))

(defstream List [xs]
  (horizontal
    (raw-string "(" {:fill :object})
    (vertically xs)
    (raw-string ")" {:fill :object})))

(defstream RandomAccess [xs]
  (horizontal
    (raw-string "[" {:fill :object})
    (items xs)
    (raw-string "]" {:fill :object})))

(defstream ISeq [xs]
  (horizontal
    (raw-string "(" {:fill :object})
    (vertically xs)
    (raw-string ")" {:fill :object})))

(prefer-method stream-dispatch IPersistentCollection RandomAccess)
(prefer-method stream-dispatch IPersistentCollection Collection)
(prefer-method stream-dispatch RandomAccess List)
(prefer-method stream-dispatch ISeq IPersistentCollection)
(prefer-method stream-dispatch ISeq Collection)

;; sets

(defstream IPersistentSet [s]
  (horizontal
    (raw-string "#{" {:fill :object})
    (items s)
    (raw-string "}" {:fill :object})))

(defstream Set [s]
  (horizontal
    (raw-string "#{" {:fill :object})
    (items s)
    (raw-string "}" {:fill :object})))

;; exceptions

(defn datafied-thrown [data]
  (let [{:keys [via trace]} data]
    (vertically
      (eduction
        (map-indexed
          (fn [i {:keys [type message at data] :as m}]
            (let [stack-trace (into []
                                    (comp
                                      (keep
                                        (fn [[prev-el el]]
                                          (when (or (nil? prev-el)
                                                    (not (and (= (prev-el 0) (el 0))
                                                              (= (prev-el 2) (el 2))
                                                              (= 'invokeStatic (prev-el 1))
                                                              (case (el 1) (invoke doInvoke invokePrim) true false))))
                                            el)))
                                      (remove
                                        (fn [^StackTraceElement el]
                                          (case (el 0)
                                            (clojure.lang.RestFn clojure.lang.AFn) true
                                            clojure.lang.Var (case (el 1) (invoke applyTo) true false)
                                            false))))
                                    (partition 2 1 (cons nil (if (= i (dec (count via))) trace [at]))))
                  n (count stack-trace)]
              (as m
                (apply
                  vertical
                  (cond->
                    [(raw-string
                       (str (when (pos? i) "Caused by ")
                            type
                            (when message (str ": " message)))
                       {:fill :error})]

                    (pos? n)
                    (conj
                      (horizontal
                        (raw-string "  " {:selectable false})
                        (let [maxn 32]
                          (apply
                            vertical
                            (-> []
                                (cond-> data (conj (override-style (single-line (stream data)) assoc :fill :error)))
                                (conj (vertically
                                        (eduction
                                          (take maxn)
                                          (map (fn [el]
                                                 (let [^String file-name (el 2)
                                                       method-name (el 1)
                                                       class-name (el 0)
                                                       line-number (el 3)
                                                       clj (if file-name
                                                             (or (.endsWith file-name ".clj")
                                                                 (.endsWith file-name ".cljc")
                                                                 (= file-name "NO_SOURCE_FILE"))
                                                             (case method-name (invoke doInvoke invokePrim invokeStatic) true false))
                                                       s (if clj
                                                           (case method-name
                                                             (invoke doInvoke invokeStatic)
                                                             (-> class-name
                                                                 str
                                                                 (Compiler/demunge)
                                                                 (.replaceFirst "/eval\\d{3,}" "/eval")
                                                                 (.replaceAll "--\\d{3,}" ""))

                                                             (str (Compiler/demunge class-name) "/" (Compiler/demunge method-name)))
                                                           (str class-name "." method-name))]
                                                   (as el
                                                     (if file-name
                                                       (horizontal
                                                         (raw-string s {:fill :error})
                                                         separator
                                                         (raw-string (str "(" file-name (when-not (neg? line-number) (str ":" line-number)) ")") {:fill :util}))
                                                       (raw-string s {:fill :error}))))))
                                          stack-trace)))
                                (cond-> (< maxn n) (conj (raw-string (str "... " (- n maxn) " more") {:fill :util}))))))))))))))
        via))))

(defn thrown [^Throwable t]
  (vertically
    (eduction
      (take-while some?)
      (map-indexed
        (fn [i ^Throwable t]
          (let [cause (.getCause t)
                stack-trace (into []
                                  (comp
                                    (keep
                                      (fn [[^StackTraceElement prev-el ^StackTraceElement el]]
                                        (when (or (nil? prev-el)
                                                  (not (and (= (.getClassName prev-el) (.getClassName el))
                                                            (= (.getFileName prev-el) (.getFileName el))
                                                            (= "invokeStatic" (.getMethodName prev-el))
                                                            (case (.getMethodName el) ("invoke" "doInvoke" "invokePrim") true false))))
                                          el)))
                                    (remove
                                      (fn [^StackTraceElement el]
                                        (case (.getClassName el)
                                          ("clojure.lang.RestFn" "clojure.lang.AFn") true
                                          "clojure.lang.Var" (case (.getMethodName el) ("invoke" "applyTo") true false)
                                          false))))
                                  (partition 2 1 (cons nil (.getStackTrace t))))
                n (count stack-trace)
                ex-data (ex-data t)]
            (as t
              (apply
                vertical
                (cond->
                  [(raw-string
                     (str (when (pos? i) "Caused by ")
                          (.getName (.getClass t))
                          (when-let [message (.getMessage t)]
                            (when-not (and cause (= message (str cause)))
                              (str ": " message))))
                     {:fill :error})]

                  (pos? n)
                  (conj
                    (horizontal
                      (raw-string "  " {:selectable false})
                      (let [maxn (if cause 1 32)]
                        (apply
                          vertical
                          (-> []
                              (cond-> ex-data (conj (override-style (single-line (stream ex-data)) assoc :fill :error)))
                              (conj (vertically
                                      (eduction
                                        (take maxn)
                                        (map (fn [^StackTraceElement el]
                                               (let [file-name (.getFileName el)
                                                     method-name (.getMethodName el)
                                                     class-name (.getClassName el)
                                                     line-number (.getLineNumber el)
                                                     clj (if file-name
                                                           (or (.endsWith file-name ".clj")
                                                               (.endsWith file-name ".cljc")
                                                               (= file-name "NO_SOURCE_FILE"))
                                                           (case method-name ("invoke" "doInvoke" "invokePrim" "invokeStatic") true false))
                                                     s (if clj
                                                         (case method-name
                                                           ("invoke" "doInvoke" "invokeStatic")
                                                           (-> class-name
                                                               (Compiler/demunge)
                                                               (.replaceFirst "/eval\\d{3,}" "/eval")
                                                               (.replaceAll "--\\d{3,}" ""))

                                                           (str (Compiler/demunge class-name) "/" (Compiler/demunge method-name)))
                                                         (str class-name "." method-name))]
                                                 (as el
                                                   (if file-name
                                                     (horizontal
                                                       (raw-string s {:fill :error})
                                                       separator
                                                       (raw-string (str "(" file-name (when-not (neg? line-number) (str ":" line-number)) ")") {:fill :util}))
                                                     (raw-string s {:fill :error}))))))
                                        stack-trace)))
                              (cond-> (< maxn n) (conj (raw-string (str "... " (- n maxn) " more") {:fill :util}))))))))))))))
      (iterate ex-cause t))))

(defstream Throwable [t]
  (let [^Throwable t t
        message (.getMessage t)
        cause (.getCause t)
        stack-trace (.getStackTrace t)
        n (count stack-trace)
        ex-data (ex-data t)]
    (horizontal
      (raw-string "(" {:fill :util})
      (apply
        vertical
        (cond->
          [(raw-string (str (.getName (class t)) ".") {:fill :object})]
          message (conj (horizontal separator (stream message)))
          ex-data (conj (horizontal separator (stream ex-data)))
          (pos? n) (conj (horizontal
                           separator
                           (raw-string "#_[" {:fill :util})
                           (vertically stack-trace)
                           (raw-string "]" {:fill :util})))
          cause (conj (horizontal separator (stream cause)))))
      (raw-string ")" {:fill :util}))))

(defstream StackTraceElement [^StackTraceElement el]
  (let [file-name (.getFileName el)
        method-name (.getMethodName el)
        class-name (.getClassName el)
        line-number (.getLineNumber el)
        s (symbol (str class-name "." method-name))]
    (if file-name
      (horizontal
        (raw-string s {:fill :symbol})
        separator
        (raw-string (str "(" file-name (when-not (neg? line-number) (str ":" line-number)) ")") {:fill :util}))
      (raw-string s {:fill :symbol}))))

;; objects

(def ^:private describe-multi
  (try
    (let [field (doto (.getDeclaredField MultiFn "name") (.setAccessible true))]
      #(symbol (.get field %)))
    (catch Exception _
      #(.-dispatchFn ^MultiFn %))))

(defstream MultiFn [^MultiFn f]
  (horizontal
    (raw-string "#reveal/multi-fn[" {:fill :object})
    (stream (describe-multi f))
    separator
    (identity-hash-code f)
    (raw-string "]" {:fill :object})))

(defstream Fn [f]
  (horizontal
    (escaped-string (.replaceAll (Compiler/demunge (.getName (class f))) "--\\d{3,}" "") {:fill :object}
                    escape-layout-chars {:fill :scalar})
    separator
    (identity-hash-code-comment f)))

(defstream Pattern [re]
  (horizontal
    (raw-string "#" {:fill :object})
    (raw-string (str \" re \") {:fill :string})
    separator
    (identity-hash-code-comment re)))

(defstream Var [var]
  (raw-string (pr-str var) {:fill :object}))

(defstream Namespace [^Namespace ns]
  (raw-string (name (.getName ns)) {:fill :object}))

(defstream Class [^Class class]
  (raw-string (.getName class) {:fill :object}))

(defstream Method [^Method meth]
  (raw-string (str meth) {:fill :object}))

(defstream Constructor [^Constructor c]
  (raw-string (str c) {:fill :object}))

(defstream Field [f]
  (raw-string (str f) {:fill :object}))

(defstream Proxy [p]
  (raw-string (str p) {:fill :object}))

(defstream Enum [^Enum enum]
  (raw-string
    (str (.getName (.getDeclaringClass enum)) "/" (.name enum))
    {:fill :scalar}))

(defstream IRef [*ref]
  (horizontal
    (raw-string "(" {:fill :util})
    (raw-string (.toLowerCase (.getSimpleName (class *ref))) {:fill :object})
    separator
    (stream @*ref)
    (raw-string ")" {:fill :util})
    separator
    (identity-hash-code-comment *ref)))

(defstream File [file]
  (horizontal
    (raw-string "(" {:fill :util})
    (raw-string "java.io.File." {:fill :object})
    separator
    (stream (str file))
    (raw-string ")" {:fill :util})))

(defstream Delay [*delay]
  (horizontal
    (raw-string "(" {:fill :util})
    (raw-string "delay" {:fill :object})
    separator
    (if (realized? *delay)
      (stream @*delay)
      (raw-string "..." {:fill :util}))
    (raw-string ")" {:fill :util})
    separator
    (identity-hash-code-comment *delay)))

(defstream Reduced [*reduced]
  (horizontal
    (raw-string "(" {:fill :util})
    (raw-string "reduced" {:fill :object})
    separator
    (stream @*reduced)
    (raw-string ")" {:fill :util})
    separator
    (identity-hash-code-comment *reduced)))

(defstream IBlockingDeref [*blocking-deref]
  (let [class-name (.getName (class *blocking-deref))]
    (cond
      (.startsWith class-name "clojure.core$promise$reify")
      (horizontal
        (raw-string "(" {:fill :util})
        (raw-string "promise" {:fill :object})
        separator
        (if (realized? *blocking-deref)
          (stream @*blocking-deref)
          (raw-string "..." {:fill :util}))
        (raw-string ")" {:fill :util})
        separator
        (identity-hash-code-comment *blocking-deref))

      (.startsWith class-name "clojure.core$future_call$reify")
      (horizontal
        (raw-string "(" {:fill :util})
        (raw-string "future" {:fill :object})
        separator
        (cond
          (.isCancelled ^Future *blocking-deref)
          (raw-string "cancelled" {:fill :util})

          (realized? *blocking-deref)
          (let [[val err] (try [@*blocking-deref nil]
                               (catch Exception e [nil e]))]
            (if err
              (override-style (stream err) assoc :fill :error)
              (stream val)))

          :else
          (raw-string "pending" {:fill :util}))
        (raw-string ")" {:fill :util})
        separator
        (identity-hash-code-comment *blocking-deref))

      :else
      (raw-string (pr-str *blocking-deref) {:fill :object}))))

(defstream Volatile [*ref]
  (horizontal
    (raw-string "(" {:fill :util})
    (raw-string "volatile!" {:fill :object})
    separator
    (stream @*ref)
    (raw-string ")" {:fill :util})
    separator
    (identity-hash-code-comment *ref)))

(defstream TaggedLiteral [x]
  (horizontal
    (raw-string "#" {:fill :object})
    (stream (:tag x))
    separator
    (stream (:form x))))

(defstream ReaderConditional [^ReaderConditional x]
  (horizontal
    (raw-string (str "#?" (when (.-splicing x) "@")) {:fill :object})
    (stream (.-form x))))

(defstream URL [x]
  (horizontal
    (raw-string "(" {:fill :util})
    (raw-string "java.net.URL." {:fill :object})
    separator
    (stream (str x))
    (raw-string ")" {:fill :util})))

(defstream URI [x]
  (horizontal
    (raw-string "(" {:fill :util})
    (raw-string "java.net.URI." {:fill :object})
    separator
    (stream (str x))
    (raw-string ")" {:fill :util})))

(defstream UUID [x]
  (horizontal
    (raw-string "#uuid" {:fill :object})
    separator
    (stream (str x))))

(defstream Eduction [eduction]
  (horizontal
    (raw-string "(" {:fill :object})
    (vertically eduction)
    (raw-string ")" {:fill :object})))

(def ^:private ^ThreadLocal utc-date-format
  (proxy [ThreadLocal] []
    (initialValue []
      (doto (SimpleDateFormat. "yyyy-MM-dd'T'HH:mm:ss.SSS-00:00")
        (.setTimeZone (TimeZone/getTimeZone "GMT"))))))

(defstream Date [^Date date]
  (let [^DateFormat format (.get utc-date-format)]
    (horizontal
      (raw-string "#inst" {:fill :object})
      separator
      (stream (.format format date)))))

(defstream Calendar [^Calendar calendar]
  (let [calendar-str (format "%1$tFT%1$tT.%1$tL%1$tz" calendar)
        minutes-index (- (.length calendar-str) 2)]
    (horizontal
      (raw-string "#inst" {:fill :object})
      separator
      (stream (str (subs calendar-str 0 minutes-index) ":" (subs calendar-str minutes-index))))))

(defstream Instant [instant]
  (horizontal
    (raw-string "#inst" {:fill :object})
    separator
    (stream (str instant))))

(defstream PersistentQueue [q]
  (horizontal
    (raw-string "#reveal/queue(" {:fill :object})
    (items q)
    (raw-string ")" {:fill :object})))

(defmacro ^:private when-class [class-name & body]
  `(try
     (Class/forName ^String ~class-name)
     ~@body
     (catch ClassNotFoundException _#)))

(when-class "java.sql.Timestamp"
  (load "stream/sql_timestamp"))

(ns/when-exists lambdaisland.deep-diff.diff
  (load "stream/deep_diff"))

(defstream Mismatch [{:keys [- +]}]
  (horizontal
    (override-style
      (horizontal
        (raw-string "-")
        (stream -))
      assoc :fill :error)
    separator
    (override-style
      (horizontal
        (raw-string "+")
        (stream +))
      assoc :fill :success)))

(defstream Insertion [{:keys [+]}]
  (override-style
    (horizontal
      (raw-string "+")
      (stream +))
    assoc :fill :success))

(defstream Deletion [{:keys [-]}]
  (override-style
    (horizontal
      (raw-string "-")
      (stream -))
    assoc :fill :error))
