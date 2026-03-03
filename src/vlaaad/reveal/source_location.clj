(ns vlaaad.reveal.source-location
  (:require [clojure.java.io :as io])
  (:import [clojure.lang Compiler Compiler$CompilerException Namespace Var]
           [java.io File]))

(set! *warn-on-reflection* true)

(defn- resolve-source-file
  ^File [file]
  (let [^File file (cond
                     (nil? file) nil
                     (instance? File file) file
                     :else (io/file (or (io/resource file) file)))]
    (when (and file (.isFile file))
      (.getAbsoluteFile file))))

(defn- parse-int
  [^String string]
  (try
    (Integer/parseInt string)
    (catch NumberFormatException _
      nil)))

(defn- make-source-location
  [file line]
  (when-some [file (resolve-source-file file)]
    {:file file
     :line (if (nat-int? line) line 1)}))

(defn- var-source-file
  ^File [var]
  (resolve-source-file (:file (meta var))))

(defn of-var
  [var]
  (let [{:keys [file line]} (meta var)]
    (make-source-location file line)))

(defn- ns-source-file
  ^File [ns]
  (some->> (cond
             (instance? Namespace ns) ns
             (symbol? ns) (find-ns ns))
           (ns-map)
           (some (fn [[_sym var]]
                   (when (and (instance? Var var)
                              (= ns (.ns ^Var var)))
                     (var-source-file var))))))

(defn of-ns
  [ns]
  (some-> (cond
            (instance? Namespace ns) ns
            (symbol? ns) (find-ns ns))
          (ns-source-file)
          (make-source-location 1)))

(defn- symbol-source-file
  ^File [sym]
  (if-some [var (resolve sym)]
    (var-source-file var)
    (if-some [ns (or (find-ns sym)
                     (some-> (namespace sym)
                             (symbol)
                             (find-ns)))]
      (ns-source-file ns))))

(defn of-symbol
  [sym]
  (if-some [var (resolve sym)]
    (of-var var)
    (of-ns sym)))

(defn- demunged-name
  ^String [^String munged-name]
  (-> (Compiler/demunge munged-name)
      (.replaceAll "--\\d{3,}" "")))

(defn- fn-symbol
  [fn]
  {:pre [(ifn? fn)]}
  (-> (class fn)
      (.getName)
      (demunged-name)
      (symbol)))

(defn of-fn
  [fn]
  (some-> (fn-symbol fn)
          (resolve)
          (of-var)))

(defn of-stack-trace-element
  [^StackTraceElement el]
  (some-> (.getClassName el)
          (demunged-name)
          (symbol)
          (symbol-source-file)
          (make-source-location (.getLineNumber el))))

(defn of-datafied-stack-trace-element
  [el]
  (when (and (vector? el)
             (= 4 (count el)))
    (let [[class-name-sym _ _ line] el]
      (when (and (symbol? class-name-sym)
                 (integer? line))
        (some-> (str class-name-sym)
                (demunged-name)
                (symbol)
                (symbol-source-file)
                (make-source-location line))))))

(defn of-compiler-exception
  [^Compiler$CompilerException e]
  (make-source-location (.-source e) (.-line e)))

(defn of-string
  [string]
  (when-some [[_ pathname line-number]
              (re-find #"([^\s\"'`]+\.clj)(?:[\:\"'`]*)?(\d+)?" string)]
    (let [file (resolve-source-file pathname)
          line (parse-int line-number)]
      (make-source-location file line))))

(defn of-test-tree-item
  [test-tree-item]
  (let [name (:name test-tree-item)]
    (when (string? name)
      (of-symbol (symbol name)))))

(defn of-value [value]
  (cond
    (var? value)
    (of-var value)

    (symbol? value)
    (of-symbol value)

    (string? value)
    (of-string value)

    (fn? value)
    (of-fn value)

    (instance? Namespace value)
    (of-ns value)

    (instance? StackTraceElement value)
    (of-stack-trace-element value)

    (vector? value)
    (of-datafied-stack-trace-element value)

    (instance? Compiler$CompilerException value)
    (of-compiler-exception value)

    (= :ctx (:type value))
    (of-test-tree-item value)))