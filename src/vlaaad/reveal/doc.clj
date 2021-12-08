(ns vlaaad.reveal.doc
  (:require [vlaaad.reveal.stream :as stream]
            [vlaaad.reveal.action :as action]
            [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [clojure.java.io :as io])
  (:import [clojure.lang Namespace RT]
           [java.io LineNumberReader InputStreamReader PushbackReader]
           [java.net URL]))

(set! *warn-on-reflection* true)

(defn parse [docstring ns]
  (let [lines (str/split-lines docstring)
        indented-lines (->> lines
                            next
                            (remove str/blank?)
                            (map #(count (take-while #{\space} %))))
        indent (if (seq indented-lines) (apply min indented-lines) 0)
        re-indent (re-pattern (str "^\\s{" indent "}"))]
    (let [ns (the-ns ns)]
      (->> lines
           (map #(str/replace % re-indent ""))
           (map (fn [line]
                  (->> line
                       (re-seq #"\[\[[^\]]+\]\]|\[[^\[]*|[^\[]*")
                       (remove str/blank?)
                       (map #(if (str/starts-with? % "[[")
                               (stream/horizontal
                                 (stream/raw-string "[[" {:fill :util})
                                 (let [sym-str (subs % 2 (- (count %) 2))
                                       sym (symbol sym-str)]
                                   (if-let [target (or (ns-resolve ns sym) (find-ns sym))]
                                     (stream/as target
                                       (stream/raw-string sym-str {:fill :object}))
                                     (stream/raw-string sym-str {:fill :string})))
                                 (stream/raw-string "]]" {:fill :util}))
                               (stream/raw-string % {:fill :string})))
                       (apply stream/horizontal))))
           stream/vertically))))

(defn- for-var [var]
  (let [m (meta var)]
    (apply
      stream/vertical
      (concat
        [(apply stream/horizontal
                (stream/stream var)
                (let [extras (cond-> []
                               (:macro m) (conj "macro")
                               (:added m) (conj (str "since " (:added m))))]
                  (when-not (empty? extras)
                    [stream/separator
                     (stream/raw-string (str "(" (str/join ", " extras) ")") {:fill :util})])))]
        (when-let [arglists (:arglists m)]
          (map stream/stream arglists))
        (when-let [deprecated (:deprecated m)]
          [stream/separator
           (if (string? deprecated)
             (stream/horizontal
               (stream/raw-string "Deprecated." {:fill :error})
               stream/separator
               (parse deprecated (:ns m)))
             (stream/raw-string "Deprecated." {:fill :error}))])
        (when-let [doc (:doc m)]
          [stream/separator
           (parse doc (:ns m))
           stream/separator])
        (when-let [forms (:forms m)]
          (cons
            (stream/raw-string "forms:" {:fill :util})
            ;; todo format as code!
            (map stream/stream forms)))
        (when-let [spec (s/get-spec var)]
          [(stream/raw-string "spec:" {:fill :util})
           ;; todo format as code!
           (stream/stream (s/describe spec))])))))

(defn- for-ns [ns]
  (let [m (meta ns)]
    (apply
      stream/vertical
      (concat
        [(stream/stream ns)]
        (when-let [author (:author m)]
          [(stream/raw-string (str "by " author) {:fill :util})])
        [stream/separator (parse (:doc m) ns)]))))

(defn- for-spec [k]
  (stream/vertical
    (stream/stream k)
    stream/separator
    (stream/raw-string "spec:" {:fill :util})
    ;; todo format as code!
    (stream/stream (s/describe k))))

(defn fn->var [fn]
  (when-let [[_ str] (->> fn
                          class
                          .getName
                          Compiler/demunge
                          (re-matches #"^([^/]*?/([^/]*?|/))(--\d\d\d\d)?$"))]
    (resolve (symbol str))))

(action/defaction ::action/doc [x]
  (cond
    (and (var? x) (:name (meta x)) (:ns (meta x)))
    #(for-var x)

    (and (instance? Namespace x) (:doc (meta x)))
    #(for-ns x)

    (and (qualified-keyword? x) (s/get-spec x))
    #(for-spec x)

    (simple-symbol? x)
    (when-let [ns (find-ns x)]
      (when (:doc (meta ns))
        #(for-ns ns)))

    (qualified-symbol? x)
    (when-let [var (resolve x)]
      #(for-var var))

    (fn? x)
    (when-let [var (fn->var x)]
      #(for-var var))))

(defn- var->source-url [var]
  (when-let [filename (:file (meta var))]
    (or (.getResource (RT/baseLoader) filename)
        (let [file (io/file filename)]
          (when (.exists file)
            (io/as-url file))))))

(defn- source [var ^URL url]
  (let [filepath (:file (meta var))]
    (with-open [reader (LineNumberReader. (InputStreamReader. (.openStream url)))]
      (dotimes [_ (dec (:line (meta var)))] (.readLine reader))
      (let [text (StringBuilder.)
            pushback-reader (proxy [PushbackReader] [reader]
                              (read []
                                (let [^PushbackReader this this]
                                  (let [i (proxy-super read)]
                                    (.append text (char i))
                                    i))))
            read-opts (if (str/ends-with? filepath "cljc") {:read-cond :allow} {})
            form (if (= :unknown *read-eval*)
                   (throw (IllegalStateException. "Unable to read source while *read-eval* is :unknown."))
                   (read read-opts (PushbackReader. pushback-reader)))]
        (stream/as form
          (stream/raw-string text {:fill :string}))))))

(action/defaction ::action/source [x]
  (cond
    (var? x)
    (when-let [url (var->source-url x)]
      #(source x url))

    (qualified-symbol? x)
    (when-let [var (resolve x)]
      (when-let [url (var->source-url var)]
        #(source var url)))

    (fn? x)
    (when-let [var (fn->var x)]
      (when-let [url (var->source-url var)]
        #(source var url)))))
