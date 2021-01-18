(ns vlaaad.reveal.prepl
  (:require [clojure.core.server :as server]
            [clojure.main :as m]
            [vlaaad.reveal.stream :as stream]
            [vlaaad.reveal.ui :as ui]
            [clojure.string :as str]))

(defn- prepl-output [x]
  (if (:exception x)
    (stream/as x
      (cond-> (stream/raw-string (-> x :val m/ex-triage m/ex-str) {:fill :error})
        (:form x)
        (as-> $ (stream/vertical
                  (stream/raw-string (:form x) {:fill :util})
                  $))))
    (case (:tag x)
      :ret (if (:vlaaad.reveal/command (:val x))
             (:val x)
             (stream/horizontal
               (stream/as x
                 (stream/vertical
                   (stream/raw-string (:form x) {:fill :util})
                   (stream/raw-string "=>" {:fill :util})))
               stream/separator
               stream/newrow
               (stream/stream (:val x))))
      :out (stream/as x (stream/raw-string (str/trim-newline (:val x)) {:fill :string}))
      :err (stream/as x (stream/raw-string (str/trim-newline (:val x)) {:fill :error}))
      :tap (if (:vlaaad.reveal/command (:val x))
             (:val x)
             (stream/horizontal
               (stream/as x (stream/raw-string "tap>" {:fill :util}))
               stream/separator
               stream/newrow
               (stream/stream (:val x))))
      (stream/stream x))))

(defn- wrap-out-fn [ui out-fn]
  (fn [x]
    (ui (prepl-output x))
    (out-fn x)))

(defn prepl [in-reader out-fn & {:keys [stdin]}]
  (let [ui (ui/make)]
    (try
      (server/prepl in-reader (wrap-out-fn ui out-fn) :stdin stdin)
      (finally (ui)))))

(defn remote-prepl
  ([] (remote-prepl {}))
  ([k v & kvs] (remote-prepl (apply hash-map k v kvs)))
  ([{:keys [host port in-reader out-fn title]
     :or {in-reader *in*
          out-fn (bound-fn* prn)}
     :as prepl-args}]
   {:pre [(some? port)]}
   (let [ui (ui/make :title (or title (str "remote-prepl on " (when host (str host ":")) port)))
         prepl-args (update prepl-args :valf (fn [valf]
                                               (or valf
                                                   #(binding [*default-data-reader-fn* tagged-literal]
                                                      (read-string %)))))]
     (try
       (apply server/remote-prepl host port in-reader (wrap-out-fn ui out-fn)
              (mapcat identity (dissoc prepl-args :host :port :in-reader :out-fn :title)))
       (finally (ui))))))

(defn io-prepl
  ([] (io-prepl {}))
  ([k v & kvs] (io-prepl (apply hash-map k v kvs)))
  ([{:keys [valf title]
     :or {valf pr-str
          title "io-prepl"}}]
   (let [ui (ui/make :title title)
         out *out*
         lock (Object.)]
     (try
       (server/prepl
         *in*
         (fn [x]
           (ui (prepl-output x))
           (binding [*out* out *flush-on-newline* true *print-readably* true]
             (locking lock
               (prn (if (#{:ret :tap} (:tag x))
                      (try
                        (assoc x :val (valf (:val x)))
                        (catch Throwable ex
                          (assoc x :val (assoc (Throwable->map ex) :phase :print-eval-result)
                                   :exception true)))
                      x))))))
       (finally (ui)))
     nil)))
