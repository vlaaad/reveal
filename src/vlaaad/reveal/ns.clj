(ns vlaaad.reveal.ns
  (:require [clojure.java.io :as io]
            [clojure.tools.reader :as reader])
  (:import [clojure.lang ExceptionInfo LineNumberingPushbackReader]
           [java.io FileNotFoundException]
           [java.net URL]))

(set! *warn-on-reflection* true)

(defmacro when-exists [ns-sym & body]
  `(try
     ;; not our problem
     (binding [*warn-on-reflection* false]
       (require '~ns-sym))
     ~@body
     (catch FileNotFoundException _#)))

(defn- read-namespaces [input-stream]
  (let [reader (LineNumberingPushbackReader. (io/reader input-stream))
        opts {:eof ::eof :read-cond :preserve}]
    ;; disable reader code execution for safety - we're unlikely to need it to parse the ns decl
    ;; note that the clojure.tools.namespace functions use reader/*read-eval*, not the *read-eval* in core
    (binding [reader/*read-eval* false]
      (loop [acc (sorted-map)]
        (let [form (try
                     (reader/read opts reader)
                     (catch ExceptionInfo e
                       (case (:type (ex-data e))
                         :reader-exception nil
                         (throw e))))]
          (if (identical? ::eof form)
            acc
            (recur
              (if-let [ns-sym (or (and (list? form)
                                       (= 'ns (first form))
                                       (= (simple-symbol? (second form)))
                                       (second form))
                                  (and (list? form)
                                       (= 'in-ns (first form))
                                       (list? (second form))
                                       (= 'quote (first (second form)))
                                       (simple-symbol? (second (second form)))
                                       (second (second form))))]
                (let [{:keys [line column]} (meta form)]
                  (assoc acc [line column] ns-sym))
                acc))))))))

(defonce ^:private source-file-info-cache-atom (atom {}))

(defn- source-file-info [^String eval-file]
  (let [^URL url (if (.contains eval-file ".jar!/")
                   (URL. (str "jar:file:" eval-file))
                   (URL. (str "file:" eval-file)))
        conn (doto (.openConnection url)
               (.setUseCaches false))]
    (try
      (let [last-modified (.getLastModified conn)
            source-file-info (@source-file-info-cache-atom eval-file)]
        (or (when (and source-file-info (= last-modified (:last-modified source-file-info)))
              source-file-info)
            (let [namespaces (read-namespaces (.getInputStream conn))
                  source-file-info {:last-modified last-modified
                                    :namespaces namespaces}]
              (swap! source-file-info-cache-atom assoc eval-file source-file-info)
              source-file-info)))
      (finally
        (.close (.getInputStream conn))))))

(defn file-ns-symbol [{:keys [^String clojure.core/eval-file line column]}]
  (when (and (string? eval-file)
             (pos-int? line)
             (pos-int? column))
    (try
      (-> eval-file
          source-file-info
          :namespaces
          (rsubseq < [line column])
          first
          (some-> val))
      (catch Exception _))))
