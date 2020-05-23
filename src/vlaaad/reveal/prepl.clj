(ns vlaaad.reveal.prepl
  (:require [clojure.core.server :as server]
            [clojure.edn :as edn]
            [clojure.main :as m]
            [vlaaad.reveal.stream :as stream]
            [vlaaad.reveal.writer-output-stream :as writer-output-stream]
            [vlaaad.reveal.ui :as ui]
            [vlaaad.reveal.style :as style]
            [clojure.string :as str])
  (:import [java.io PrintStream]))

(defn- line-print-stream [line-fn]
  (let [sb (StringBuilder.)]
    (-> #(doseq [^char ch %]
           (if (= \newline ch)
             (let [str (.toString sb)]
               (.delete sb 0 (.length sb))
               (line-fn str))
             (.append sb ch)))
        (PrintWriter-on nil)
        (writer-output-stream/make)
        (PrintStream. true "UTF-8"))))

(defn- wrap-err-out [f ui]
  (fn []
    (let [out System/out
          err System/err]
      (System/setOut (line-print-stream #(do
                                           (.println out %)
                                           (-> % stream/system-out ui))))
      (System/setErr (line-print-stream #(do (.println err %)
                                             (-> % stream/system-err ui))))
      (f)
      (System/setOut out)
      (System/setErr err))))

(defn prepl-output [x]
  (stream/as x
    (if (:exception x)
      (cond-> (stream/raw-string (-> x :val m/ex-triage m/ex-str) {:fill style/error-color})
              (:form x)
              (as-> err-output
                    (stream/vertical
                      (stream/raw-string (:form x) {:fill style/util-color})
                      err-output)))
      (case (:tag x)
        :ret (stream/vertical
               (stream/raw-string (:form x) {:fill style/util-color})
               (stream/horizontal
                 (stream/raw-string "=>" {:fill style/util-color})
                 stream/separator
                 (stream/stream (:val x))))
        :out (stream/raw-string (str/trim-newline (:val x)) {:fill style/string-color})
        :err (stream/raw-string (str/trim-newline (:val x)) {:fill style/error-color})
        :tap (stream/horizontal
               (stream/raw-string "tap>" {:fill style/util-color})
               stream/separator
               (stream/stream (:val x)))
        (stream/emit x)))))

(defn start [& {:keys [prepl streams]
                :or {prepl server/prepl
                     streams true}}]
  (let [out *out*
        err *err*
        ui (ui/make)
        repl (-> #(prepl *in* (fn [x]
                                (ui (prepl-output x))
                                (binding [*out* out]
                                  (if (:exception x)
                                    (binding [*out* err]
                                      (println (m/ex-str (m/ex-triage (:val x))))
                                      (flush))
                                    (do
                                      (case (:tag x)
                                        :out (println (:val x))
                                        :err (binding [*out* err]
                                               (println (:val x)))
                                        (:tap :ret) (prn (:val x))
                                        (prn x))
                                      (flush)))
                                  (print (str (:ns x *ns*) "=> "))
                                  (flush))))
                 (cond-> streams (wrap-err-out ui)))
        v (str "Clojure " (clojure-version))]
    (ui (stream/as *clojure-version*
          (stream/raw-string v {:fill style/util-color})))
    (println v)
    (print (str (.name *ns*) "=> "))
    (flush)
    (repl)
    nil))

(defn -main [& args]
  (apply start (map edn/read-string args)))
