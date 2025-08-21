(ns vlaaad.reveal.ns
  (:require [clojure.java.io :as io]
            [clojure.tools.namespace.file :as namespace.file]
            [clojure.tools.namespace.parse :as namespace.parse]
            [clojure.tools.reader :as reader])
  (:import [clojure.lang ExceptionInfo]
           [java.io FileNotFoundException IOException]))

(defmacro when-exists [ns-sym & body]
  `(try
     ;; not our problem
     (binding [*warn-on-reflection* false]
       (require '~ns-sym))
     ~@body
     (catch FileNotFoundException _#)))

(defn- try-read-file-ns-decl [source-file]
  ;; disable reader code execution for safety - we're unlikely to need it to parse the ns decl
  ;; note that the clojure.tools.namespace functions use reader/*read-eval*, not the *read-eval* in core
  (binding [reader/*read-eval* false]
    (try
      (namespace.file/read-file-ns-decl source-file)
      (catch ExceptionInfo e
        (case (:ex-kind (ex-data e))
          (:reader-error) nil ; syntax error or reader/*read-eval* violation
          (:eof :illegal-argument) nil ; syntax error
          (throw e)))
      (catch IOException _
        nil))))

(defonce ^:private source-file-info-cache-atom (atom {}))

(defn- source-file-info [source-file-or-path]
  (let [source-file (io/file source-file-or-path)]
    (when (and source-file (.isFile source-file))
      (let [absolute-path (.getAbsolutePath source-file)
            last-modified (.lastModified source-file)]
        (or (when-let [source-file-info (get @source-file-info-cache-atom absolute-path)]
              (when (= last-modified (:last-modified source-file-info))
                source-file-info))
            (let [ns-decl (try-read-file-ns-decl source-file)
                  ns-symbol (some-> ns-decl namespace.parse/name-from-ns-decl)]
              (-> (swap! source-file-info-cache-atom
                         assoc absolute-path
                         {:last-modified last-modified
                          :ns-symbol ns-symbol})
                  (get absolute-path))))))))

(defn file-ns-symbol [source-file-or-path]
  (some-> source-file-or-path source-file-info :ns-symbol))
