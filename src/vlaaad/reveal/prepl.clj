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
      :ret (if (ui/command? (:val x))
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
      :tap (if (ui/command? (:val x))
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
  (let [ui (ui/make-queue)]
    (try
      (server/prepl in-reader (wrap-out-fn ui out-fn) :stdin stdin)
      (finally (ui)))))

(defn remote-prepl
  ([] (remote-prepl {}))
  ([k v & kvs] (remote-prepl (apply hash-map k v kvs)))
  ([{:keys [host port in-reader out-fn]
     :or {in-reader *in*
          out-fn prn}
     :as opts}]
   {:pre [(some? port)]}

   (let [ui (-> opts
                (select-keys [:title :close-difficulty :always-on-top :decorations :bounds])
                (update :title #(or % (str "remote-prepl on " (when host (str host ":")) port)))
                (update :bounds #(or % 'vlaaad.reveal.ui/repl))
                ui/make-queue)
         prepl-args (-> opts
                        (select-keys [:host :port :in-reader :out-fn :valf :readf])
                        (update :valf (fn [valf]
                                        (or valf
                                            #(binding [*suppress-read* true
                                                       *default-data-reader-fn* tagged-literal]
                                               (read-string %))))))]
     (try
       (apply server/remote-prepl host port in-reader (wrap-out-fn ui (bound-fn* out-fn))
              (mapcat identity (dissoc prepl-args :host :port :in-reader :out-fn)))
       (finally (ui))))))

(defn io-prepl
  ([] (io-prepl {}))
  ([k v & kvs] (io-prepl (apply hash-map k v kvs)))
  ([{:keys [valf title]
     :or {valf pr-str
          title "io-prepl"}
     :as args}]
   (let [ui (-> args
                (dissoc :valf)
                (update :title #(or % "io-prepl"))
                (update :bounds #(or % 'vlaaad.reveal.ui/repl))
                ui/make-queue)
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
