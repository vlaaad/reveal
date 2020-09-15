(ns vlaaad.reveal.doc
  (:require [vlaaad.reveal.stream :as stream]
            [vlaaad.reveal.action :as action]
            [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [clojure.java.io :as io])
  (:import [com.vladsch.flexmark.parser Parser]
           [com.vladsch.flexmark.util.data MutableDataSet]
           [com.vladsch.flexmark.util.ast Node Document]
           [com.vladsch.flexmark.ast Paragraph SoftLineBreak HardLineBreak ThematicBreak
                                     Text BulletList ListItem Heading BlockQuote
                                     FencedCodeBlock IndentedCodeBlock
                                     OrderedList DelimitedNodeImpl HtmlInline TextBase
                                     AutoLink InlineLinkNode MailLink]
           [com.vladsch.flexmark.ext.wikilink WikiLink WikiLinkExtension]
           [com.vladsch.flexmark.ext.autolink AutolinkExtension]
           [clojure.lang Namespace RT]
           [java.io LineNumberReader InputStreamReader PushbackReader]
           [java.net URL]))

(def ^:dynamic ^:private *doc-ns*)

(def ^Parser parser
  (.build (Parser/builder
            (doto (MutableDataSet.)
              (.set Parser/EXTENSIONS [(WikiLinkExtension/create)
                                       (AutolinkExtension/create)])))))

(set! *warn-on-reflection* true)

(defprotocol md->sf
  (->sf [this]))

(defn- children [^Node node]
  (->> node
       .getFirstChild
       (iterate #(.getNext ^Node %))
       (take-while some?)))

(defn- line-break? [x]
  (or (instance? SoftLineBreak x)
      (instance? HardLineBreak x)))

(defn- partition-lines [xs]
  (->> xs
       (partition-by line-break?)
       (remove #(-> % first line-break?))))

(extend-protocol md->sf
  Node
  (->sf [this]
    (stream/raw-string (.getChars this) {:fill :string}))
  Document
  (->sf [this]
    (apply stream/vertical (map ->sf (children this))))
  Paragraph
  (->sf [this]
    (cond-> (->> this
                 children
                 partition-lines
                 (map (fn [children]
                        (apply stream/horizontal (map ->sf children))))
                 (apply stream/vertical))
            (.isTrailingBlankLine this)
            (stream/horizontal (stream/raw-string "\n"))))
  Text
  (->sf [this]
    (stream/raw-string (.getChars this) {:fill :string}))
  TextBase
  (->sf [this]
    (apply stream/horizontal (map ->sf (children this))))
  AutoLink
  (->sf [this]
    (let [link (str (.getChars this))]
      (stream/as link
        (stream/raw-string link {:fill :scalar}))))
  MailLink
  (->sf [this]
    (stream/raw-string (.getChars this) {:fill :string}))
  InlineLinkNode
  (->sf [this]
    (stream/horizontal
      (stream/raw-string (.getTextOpeningMarker this) {:fill :util})
      (stream/raw-string (.getText this) {:fill :string})
      (stream/raw-string (str (.getTextClosingMarker this)
                              (.getLinkOpeningMarker this))
                         {:fill :util})
      (stream/as (str (.getUrl this))
        (stream/raw-string (.getUrl this) {:fill :scalar}))
      (stream/raw-string (str (.getLinkClosingMarker this)) {:fill :util})))
  DelimitedNodeImpl
  (->sf [this]
    (let [marker (str (.getOpeningMarker this))
          marker-color (case marker
                         "*" :string
                         :util)]
      (stream/horizontal
        (stream/raw-string marker {:fill marker-color})
        (stream/raw-string (.getText this) {:fill (case marker
                                                    "`" :symbol
                                                    :string)})
        (stream/raw-string (.getClosingMarker this) {:fill marker-color}))))
  ThematicBreak
  (->sf [this]
    (stream/raw-string (.getChars this) {:fill :string}))
  BulletList
  (->sf [this]
    (let [marker (str (.getOpeningMarker this) " ")]
      (->> this
           children
           (map (fn [child]
                  (stream/horizontal
                    (stream/raw-string marker {:fill :string})
                    (->sf child))))
           (apply stream/vertical))))
  OrderedList
  (->sf [this]
    (->> this
         children
         (map (fn [i child]
                (stream/horizontal
                  (stream/raw-string (str i (.getDelimiter this) " ") {:fill :string})
                  (->sf child)))
              (iterate inc (.getStartNumber this)))
         (apply stream/vertical)))
  ListItem
  (->sf [this]
    (->> this children (map ->sf) (apply stream/vertical)))
  Heading
  (->sf [this]
    (stream/horizontal
      (stream/raw-string (str (.getOpeningMarker this) " ") {:fill :util})
      (->> this children (map ->sf) (apply stream/horizontal))
      (stream/raw-string "\n")))
  BlockQuote
  (->sf [this]
    (stream/horizontal
      (stream/raw-string "> " {:fill :util})
      (apply stream/horizontal (map ->sf (children this)))))
  FencedCodeBlock
  (->sf [this]
    (stream/vertical
      (stream/raw-string (str (.getOpeningFence this) (.getInfo this)) {:fill :util})
      (stream/raw-string (str/trim-newline (.getChildChars this)) {:fill :symbol})
      (stream/raw-string (.getClosingFence this) {:fill :util})))
  IndentedCodeBlock
  (->sf [this]
    (stream/raw-string (str (.getChars this)) {:fill :symbol}))
  HtmlInline
  (->sf [this]
    (stream/raw-string (str (.getChars this)) {:fill :util}))
  WikiLink
  (->sf [this]
    (stream/horizontal
      (stream/raw-string (.getOpeningMarker this) {:fill :util})
      (if-let [var (ns-resolve *doc-ns* (symbol (str (.getChildChars this))))]
        (stream/as var
          (stream/raw-string
            (.getChildChars this)
            {:fill :object}))
        (stream/raw-string (.getChildChars this) {:fill :string}))
      (stream/raw-string (.getClosingMarker this) {:fill :util}))))

(defn parse [docstring ns]
  (let [lines (str/split-lines docstring)
        indented-lines (->> lines
                            next
                            (remove str/blank?)
                            (map #(count (take-while #{\space} %))))
        indent (if (seq indented-lines) (apply min indented-lines) 0)
        re-indent (re-pattern (str "^\\s{" indent "}"))]
    (binding [*doc-ns* (the-ns ns)]
      (stream/as-is
        (->sf (.parse parser ^String (->> lines
                                          (map #(str/replace % re-indent ""))
                                          (str/join "\n"))))))))

(defn- for-var [var]
  (let [m (meta var)]
    (stream/as-is
      (apply
        stream/vertical
        (concat
          [(apply stream/horizontal
                  (stream/stream var)
                  (when (:macro m)
                    [stream/separator
                     (stream/raw-string "(macro)" {:fill :util})]))]
          (when-let [arglists (:arglists m)]
            (map stream/stream arglists))
          (when-let [deprecated (:deprecated m)]
            [stream/separator
             (if (string? deprecated)
               (stream/horizontal
                 (stream/raw-string "Deprecated." {:fill :string})
                 stream/separator
                 (parse deprecated (:ns m)))
               (stream/raw-string "Deprecated." {:fill :string}))])
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
             (stream/stream (s/describe spec))]))))))

(defn- for-ns [ns]
  (stream/as-is
    (stream/vertical
      (stream/stream ns)
      stream/separator
      (parse (:doc (meta ns)) ns))))

(defn- for-spec [k]
  (stream/as-is
    (stream/vertical
      (stream/stream k)
      stream/separator
      (stream/raw-string "spec:" {:fill :util})
      ;; todo format as code!
      (stream/stream (s/describe k)))))

(defn fn->var [fn]
  (when-let [[_ str] (->> fn
                          class
                          .getName
                          Compiler/demunge
                          (re-matches #"^([^/]*?/([^/]*?|/))(--\d\d\d\d)?$"))]
    (resolve (symbol str))))

(action/defaction ::doc [x]
  (cond
    (and (var? x) (:name (meta x)) (:ns (meta x)))
    #(for-var x)

    (and (instance? Namespace x) (:doc (meta x)))
    #(for-ns x)

    (and (qualified-keyword? x) (s/get-spec x))
    #(for-spec x)

    (simple-symbol? x)
    (when-let [ns (find-ns x)]
      (when (:doc (meta x))
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
        (stream/as-is
          (stream/as form
            (stream/raw-string text {:fill :string})))))))

(action/defaction ::source [x]
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
