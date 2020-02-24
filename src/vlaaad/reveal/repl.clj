(ns vlaaad.reveal.repl
  (:require [clojure.main :as m]
            [vlaaad.reveal.ui :as ui]
            [vlaaad.reveal.style :as style]
            [vlaaad.reveal.stream :as stream]
            [clojure.string :as str]))

(defn stream-read [form ui]
  (ui (stream/as form
        (stream/raw-string (pr-str form) {:fill ::style/util-color}))))

(defn wrap-read [ui read]
  (fn [request-prompt request-exit]
    (let [ret (read request-prompt request-exit)]
      (condp = ret
        request-exit request-exit
        :repl/quit request-exit
        request-prompt request-prompt
        (doto ret (stream-read ui))))))

(defn wrap-print [ui print]
  (fn [x]
    (ui
      (stream/just
        (stream/horizontal
          (stream/raw-string "=>" {:fill ::style/util-color})
          stream/separator
          (stream/raw-string " " {:fill ::style/util-color})
          (stream/stream x))))
    (print x)))

(defn wrap-caught [ui caught]
  (fn [ex]
    (ui (stream/as ex
          (stream/raw-string (-> ex Throwable->map m/ex-triage m/ex-str)
                             {:fill ::style/error-color})))
    (caught ex)))

(defn make-tap [ui]
  (fn [x]
    (ui (stream/just
          (stream/horizontal
            (stream/raw-string "tap>" {:fill ::style/util-color})
            stream/separator
            (stream/raw-string " " {:fill ::style/util-color})
            (stream/stream x))))))

(defn make-print [ui out fill]
  (PrintWriter-on
    #(do
       (ui (stream/as %
             (stream/raw-string (str/trim-newline %)
                                {:fill fill})))
       (binding [*out* out]
         (print %)))
    nil))

(defn -main [& args]
  (let [ui (ui/make)
        tap (make-tap ui)
        out *out*
        err *err*
        ui-out (PrintWriter-on
                 #(do
                    (ui (stream/as %
                          (stream/raw-string (str/trim-newline %) {:fill ::style/string-color})))
                    (binding [*out* out]
                      (print %)))
                 nil)
        ui-err (PrintWriter-on
                 #(do
                    (ui (stream/as %
                          (stream/raw-string (str/trim-newline %) {:fill ::style/error-color})))
                    (binding [*out* err]
                      (print %)
                      (flush)))
                 nil)
        version (str "Clojure " (clojure-version))]
    (ui (stream/as *clojure-version*
          (stream/raw-string version {:fill ::style/util-color})))
    (add-tap tap)
    (println version)
    (m/repl :init #(apply require m/repl-requires)
            :read (wrap-read ui m/repl-read)
            :eval #(binding [*out* ui-out
                             *err* ui-err]
                     (let [ret (eval %)]
                       (flush)
                       ret))
            :print (wrap-print ui prn)
            :caught (wrap-caught ui m/repl-caught))
    (remove-tap tap)
    (ui)))
