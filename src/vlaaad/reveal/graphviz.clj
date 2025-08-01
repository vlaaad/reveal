(ns vlaaad.reveal.graphviz
  (:require [cljfx.ext.web-view :as fx.ext.web-view]
            [cljfx.fx.web-view :as fx.web-view]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [vlaaad.reveal.action :as action]
            [vlaaad.reveal.style :as style]
            [vlaaad.reveal.view :as view])
  (:import [clojure.lang IRef]
           [javafx.scene.input KeyCode KeyEvent]
           [javafx.scene.web WebView]))

(def ^:private template
  "<head>
     <script src=\"%s\"></script>
     <script src=\"%s\"></script>
     <style>
       body {background-color:%s;margin:0px}
     </style>
   </head>
   <body>
     <script>
       new Viz().renderSVGElement(%s).then(function(element) {
           document.body.appendChild(element);
         }).catch(function(error) {
           var div = document.createElement('div');
           div.textContent = error;
           div.style.color = '%s';
           document.body.appendChild(div);
         });
     </script>
   </body>")

(def ^:private injection
  (let [bg-color (pr-str @style/background-color)
        font-color (pr-str (style/color :symbol))
        util-color (pr-str (style/color :util))]
    (str "{" ;; part of replacement injection
         "graph[bgcolor = " bg-color ", color = " util-color ", fontcolor = " font-color "];"
         "node[fontcolor = " font-color ", color = " util-color "];"
         "edge[fontcolor = " font-color ", color = " util-color "];")))

(defn- inject-default-style [s]
  (str/replace-first s "{" injection))

(defn- handle-key-pressed [^KeyEvent e]
  (when (.isShortcutDown e)
    (condp contains? (.getCode e)
      #{KeyCode/MINUS KeyCode/SUBTRACT}
      (do (.consume e)
          (let [^WebView v (.getTarget e)]
            (.setZoom v (- (.getZoom v) 0.1))))
      #{KeyCode/EQUALS KeyCode/PLUS KeyCode/ADD}
      (do (.consume e)
          (let [^WebView v (.getTarget e)]
            (.setZoom v (+ (.getZoom v) 0.1))))
      nil)))

(defn view [{:keys [graph]}]
  {:fx/type fx.ext.web-view/with-engine-props
   :props {:content (format template
                            (.toExternalForm (io/resource "vlaaad/reveal/graphviz/viz.js"))
                            (.toExternalForm (io/resource "vlaaad/reveal/graphviz/full.render.js"))
                            @style/background-color
                            (pr-str (inject-default-style graph))
                            (style/color :error))}
   :desc {:fx/type fx.web-view/lifecycle
          :on-key-pressed handle-key-pressed
          :context-menu-enabled false}})


(defn- graphviz-string? [x]
  (and (string? x)
       (or (str/starts-with? x "graph")
           (str/starts-with? x "digraph"))))

(action/defaction ::action/graphviz [x]
  (cond
    (graphviz-string? x)
    (fn [] {:fx/type view :graph x})

    (and (instance? IRef x) (graphviz-string? @x))
    (fn []
      {:fx/type view/observable-view
       :ref x
       :fn #(array-map :fx/type view :graph %)})))