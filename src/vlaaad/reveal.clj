(ns vlaaad.reveal
  (:require [clojure.core.server :as server]
            [clojure.string :as str]
            [clojure.edn :as edn]))

(defn ^{:doc "Start repl that wraps [[clojure.main/repl]]

              Evaluating `:repl/quit` will quit the repl. Accepts same arguments
              as [[clojure.main/repl]]"
        :cli "Start repl

              Evaluating :repl/quit will quit the repl"}
  repl
  [& {:as args}]
  ((requiring-resolve 'vlaaad.reveal.repl/repl) args))

(defn prepl
  "Start repl with prepl backend

  Accepted arguments:
  - `:backend` (optional) - prepl backend, a function of 2 args: `in-reader` and
    `out-fn`, as per [[clojure.core.server/prepl]]"
  [& {:as args}]
  ((requiring-resolve 'vlaaad.reveal.prepl/prepl) args))

(defn ^{:doc "Start repl with remote prepl backend

              Accepted arguments:
              - `:port` (required) - target port
              - `:host` (optional) - target host"
        :cli "Start repl with remote prepl backend

              Accepted arguments:
              - :port (required) - target port
              - :host (optional) - target host

              Examples:
              - using remote prepl on the localhost:
                  remote-prepl :port 5555
              - using remote prepl over the network:
                  remote-prepl :host 192.168.1.15 :port 5555"}
  remote-prepl
  [& {:keys [host port] :as args}]
  {:pre [(some? port)]}
  ((requiring-resolve 'vlaaad.reveal.prepl/prepl)
   (assoc args :backend (fn [in-reader out-fn]
                          (server/remote-prepl host port in-reader out-fn
                                               :valf #(binding [*default-data-reader-fn* tagged-literal]
                                                        (read-string %)))))))

(defn ui
  "Create and show Reveal UI window

  Returns multi-arity function:
  - when called with 1 arg, will add supplied value to main output panel
  - when called with 0 args, will close and dispose the window"
  []
  ((requiring-resolve 'vlaaad.reveal.ui/make)))

(defn- ^{:cli "Display help for a specific command"} help [command]
  (let [doc (or (:cli (meta ((ns-interns 'vlaaad.reveal) (symbol command))))
                (throw (RuntimeException. (str "Unknown command: " command))))
        lines (str/split-lines doc)
        indented-lines (->> lines
                            next
                            (remove str/blank?)
                            (map #(count (take-while #{\space} %))))
        indent (if (seq indented-lines) (apply min indented-lines) 0)
        re-indent (re-pattern (str "^\\s{" indent "}"))]
    (->> lines
         (map #(str/replace % re-indent ""))
         (run! println))))

(defn -main [& [command & args]]
  (if (contains? #{nil "-?" "-h" "--help"} command)
    (do (println "Command line entry point for launching reveal socket repl\n\nAvailable commands:")
        (let [sym+vars (->> (ns-interns 'vlaaad.reveal)
                            (filter #(-> % val meta :cli))
                            (sort-by #(-> % val meta :line)))
              pad (apply max (map #(-> % key name count) sym+vars))]
          (doseq [[sym var] sym+vars]
            (println (format (str "  %-" pad "s  %s") sym (first (str/split-lines (:cli (meta var))))))))
        (println "\nUse help <command> to see full description of that command"))
    (apply (or ((ns-interns 'vlaaad.reveal) (symbol command))
               (throw (RuntimeException. (str "Unknown command: " command))))
           (map #(try (let [ret (edn/read-string %)]
                        (if (symbol? ret) % ret))
                      (catch Exception _ %))
                args))))
