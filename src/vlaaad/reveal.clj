(ns vlaaad.reveal
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

(defn- update-some [m k f & args]
  (let [v (get m k ::not-found)]
    (if (= v ::not-found)
      m
      (assoc m k (apply f v args)))))

(defn- resolve-fn [x]
  (if (symbol? x)
    @(requiring-resolve x)
    x))

(defn- hydrate-fns [m & ks]
  (reduce #(update-some %1 %2 resolve-fn) m ks))

(defn ^{:doc "Start reveal repl that wraps [[clojure.main/repl]]

              Evaluating `:repl/quit` will quit the repl.

              Accepts same arguments as [[clojure.main/repl]] - `:init`,
              `:need-prompt`, `:prompt`, `:flush`, `:read`, `:eval`, `:print`
              and `:caught` - plus:
              - `:title` (optional, defaults to \"repl\") - window title"
        :cli "Start reveal repl that wraps clojure.main/repl

              Evaluating :repl/quit will quit the repl

              Accepted args (all should be qualified ns symbols, as per
              clojure.main/repl documentation) - :init, :need-prompt, :prompt,
              :flush, :read, :eval, :print and :caught - plus:
              - :title (optional, defaults to \"repl\") - window title

              Examples:
              - starting the repl:
                  clj ... repl
              - starting the repl that pretty-prints its textual output:
                  clj ... repl :print clojure.pprint/pprint"}
  repl
  ([] (repl {}))
  ([k v & kvs] (repl (apply hash-map k v kvs)))
  ([args]
   ((requiring-resolve 'vlaaad.reveal.repl/repl)
    (hydrate-fns args :init :need-prompt :prompt :flush :read :eval :print :caught))))

(defn ^{:doc "Start reveal prepl bound to [[*in*]] and [[*out*]]

              Evaluating `:repl/quit` will quit the prepl

              Accepted arguments:
              - `:valf` (optional, defaults to [[pr-str]]) - 1 arg function that
                post-processes returned and tapped values
              - `:title` (optional, defaults to \"io-prepl\") - window title"
        :cli "Start reveal prepl bound to *in* and *out*

              Evaluating :repl/quit will quit the prepl

              Accepted arguments:
              - :valf (optional, defaults to clojure.core/pr-str) - qualified
                fn symbol to a 1-arg function that post-processes returned and
                tapped values
              - :title (optional, defaults to \"io-prepl\") - window title

              Examples:
              - starting the prepl:
                  clj ... io-prepl"}
  io-prepl
  ([] (io-prepl {}))
  ([k v & kvs] (io-prepl (apply hash-map k v kvs)))
  ([args]
   ((requiring-resolve 'vlaaad.reveal.prepl/io-prepl) (hydrate-fns args :valf))))

(defn ^{:doc "Start a client reveal prepl that connects to a remote prepl

              Reveal is more useful when it runs in the target process, having
              full access to objects in the VM. With remote prepl, it can only
              access data received from the network. It is useful for e.g.
              connecting to remote JVMs that don't have reveal on the classpath
              or to ClojureScript prepls.

              Accepted arguments:
              - `:port` (required) - target port
              - `:host` (optional, defaults to localhost) - target host
              - `:in-reader` (optional, defaults to [[*in*]]) - a LineNumberingPushbackReader
                to read forms from
              - `:out-fn` (optional, defaults to [[prn]]) - 1-arg function that
                receives prepl data
              - `:valf` (optional, defaults to edn string reader with
                [[tagged-literal]] support) - 1-arg function that post-processes
                returned and tapped values
              - `:readf` (optional, default `#(read %1 false %2)`) - a 2-arg
                function of in-reader and EOF value that reads from in-reader
              - `:title` (optional, default \"remote-prepl on [host:]port\") -
                window title"
        :cli "Start a client reveal prepl that connects to a remote prepl

              Reveal is more useful when it runs in the target process, having
              full access to objects in the VM. With remote prepl, it can only
              access data received from the network. It is useful e.g. for
              connecting to remote JVMs that don't have reveal on the classpath
              or to ClojureScript prepls.

              Accepted arguments:
              - :port (required) - target port
              - :host (optional, defaults to localhost) - target host
              - :in-reader (optional, defaults to clojure.core/*in*) - qualified
                symbol to a LineNumberingPushbackReader to read forms from
              - :out-fn (optional, defaults to clojure.core/prn) - qualified fn
                symbol to a 1-arg function that receives prepl data
              - :valf (optional, defaults to edn string reader with tagged
                literal support) - qualified fn symbol to a 1-arg function that
                post-processes returned and tapped values
              - :readf (optional) - qualified fn symbol to a 2-arg function of
                in-reader and EOF value that reads from in-reader
              - :title (optional, default \"remote-prepl on [host:]port\") -
                window title

              Examples:
              - using remote prepl on the localhost:
                  clj ... remote-prepl :port 5555
              - using remote prepl over the network:
                  clj ... remote-prepl :host '\"192.168.1.15\"' :port 5555
              - using remote prepl without value deserialization:
                  clj ... remote-prepl :port 5555 :valf clojure.core/identity
              - using remote prepl with custom window title:
                  clj ... remote-prepl :port 5555 :title '\"cljs-dev\"'"}
  remote-prepl
  ([] (remote-prepl {}))
  ([k v & kvs] (remote-prepl (apply hash-map k v kvs)))
  ([args]
   ((requiring-resolve 'vlaaad.reveal.prepl/remote-prepl)
    (hydrate-fns args :valf :readf :in-reader :out-fn))))

(defn ui
  "Create and show Reveal UI window

  Returns multi-arity function:
  - when called with 1 arg:
    - if the arg contains `:vlaaad.reveal/command` key, executes UI command
    - otherwise, adds supplied value to main output panel
  - when called with 0 args, will close and dispose the window

  Reveal command is either a value produced from command function in
  [[vlaaad.reveal.ext]] ns or a map with these keys:
  - `:vlaaad.reveal/command` - a code form that will be evaluated by Reveal
  - `:env` (optional) - a map from symbols to arbitrary values that will be
    resolvable in evaluated command form
  - `:ns` (optional, default `'vlaaad.reveal.ext`) - ns used for evaluation

  Accepted args:
  - `:title` (optional) - window title"
  ([] (ui {}))
  ([k v & kvs] (ui (apply hash-map k v kvs)))
  ([args]
   ((requiring-resolve 'vlaaad.reveal.ui/make) args)))

(defn- ^{:cli "Display help for a specific command"} help [command]
  (let [doc (or (:cli (meta ((ns-interns 'vlaaad.reveal) command)))
                (throw (Exception. (str "Unknown command: " command))))
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
    (do (println "Command line entry point for launching reveal repls\n\nAvailable commands:")
        (let [sym+vars (->> (ns-interns 'vlaaad.reveal)
                            (filter #(-> % val meta :cli))
                            (sort-by #(-> % val meta :line)))
              pad (apply max (map #(-> % key name count) sym+vars))]
          (doseq [[sym var] sym+vars]
            (println (format (str "  %-" pad "s  %s") sym (first (str/split-lines (:cli (meta var))))))))
        (println "\nUse 'clj ... help <command>' to see full description of that command"))
    (apply (or (let [var ((ns-interns 'vlaaad.reveal) (symbol command))]
                 (and (:cli (meta var)) var))
               (throw (Exception. (str "Unknown command: " command))))
           (map edn/read-string args))))
