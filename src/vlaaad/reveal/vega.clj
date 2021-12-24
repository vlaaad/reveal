(ns vlaaad.reveal.vega
  (:require [clojure.java.io :as io]
            [cljfx.ext.web-view :as fx.ext.web-view]
            [clojure.data.json :as json]
            [cljfx.api :as fx]
            [vlaaad.reveal.ui :as ui]
            [clojure.string :as str]
            [cljfx.prop :as fx.prop]
            [cljfx.mutator :as fx.mutator]
            [cljfx.lifecycle :as fx.lifecycle]
            [vlaaad.reveal.style :as style]
            [vlaaad.reveal.font :as font])
  (:import [javafx.scene.web WebView]
           [javafx.concurrent Worker$State Worker]
           [javafx.beans.value ChangeListener]))

(defn- html [spec opt]
  (format
    "
   <head>
     <script src=\"%s\"></script>
     <script src=\"%s\"></script>
     <script src=\"%s\"></script>
     <style>
       html, body {height:100%%;margin:0;background-color:%s;}
       #wrap {height:100vh;width:100%%;display:flex;flex-direction:column;}
       #view {flex:1;overflow:auto;}
     </style>
   </head>
   <body>
     <div id=\"wrap\">
       <div id=\"bind\"></div>
       <div id=\"view\"></div>
     </div>
     <script>
       var viewPromise = vegaEmbed(\"#view\", %s, %s).then(function(res) {return res.view;});
     </script>
   </body>
   "
    (.toExternalForm (io/resource "vlaaad/reveal/vega/vega@5.21.0.min.js"))
    (.toExternalForm (io/resource "vlaaad/reveal/vega/vega-lite@5.2.0.min.js"))
    (.toExternalForm (io/resource "vlaaad/reveal/vega/vega-embed@6.20.5.min.js"))
    @style/background-color
    (json/write-str spec)
    (json/write-str opt)))

(defn- create-popup [_]
  (let [view (WebView.)]
    (ui/inspect
      {:fx/type fx/ext-instance-factory
       :create (constantly view)}
      :bounds ::popup)
    (.getEngine view)))

(defmacro on-loaded [worker-expr & body]
  `(let [^Worker worker# ~worker-expr]
     (if (= Worker$State/SUCCEEDED (.getState worker#))
       (do ~@body)
       (.addListener
         (.stateProperty worker#)
         (reify ChangeListener
           (~'changed [this# prop# _# new#]
             (when (= Worker$State/SUCCEEDED new#)
               (.removeListener prop# this#)
               ~@body)))))))

(defn- set-data-html [m]
  (format
    "viewPromise = viewPromise.then(function(view) {
      return view
        %s
        .resize()
        .runAsync()
        .then(function() { return view; });
     })"
    (->> m
         (map (fn [[k v]]
                (str ".change("
                     (json/write-str k)
                     ", vega.changeset().remove(vega.truthy).insert("
                     (json/write-str v)
                     "))")))
         (str/join "\n        "))))

(defn- set-signal-html [m]
  (format
    "viewPromise = viewPromise.then(function(view) {
      return view
        %s
        .runAsync()
        .then(function() { return view; });
     })"
    (->> m
         (map (fn [[k v]]
                (str ".signal("
                     (json/write-str k)
                     ", "
                     (json/write-str v)
                     ")")))
         (str/join "\n        "))))

(defn- replace-from-map-mutator [f default]
  (reify fx.mutator/Mutator
    (assign! [_ instance coerce value]
      (f instance (coerce value)))
    (replace! [_ instance coerce old-value new-value]
      (when-not (= old-value new-value)
        (f instance (coerce (merge (zipmap (keys old-value) (repeat default)) new-value)))))
    (retract! [_ instance coerce value]
      (f instance (coerce (zipmap (keys value) (repeat [])))))))

(def ext-with-data-props
  (fx/make-ext-with-props
    {:data (fx.prop/make (replace-from-map-mutator
                           (fn [^WebView view m]
                             (when-not (empty? m)
                               (let [e (.getEngine view)]
                                 (on-loaded
                                   (.getLoadWorker e)
                                   (.executeScript (.getEngine view) (set-data-html m))))))
                           [])
                         fx.lifecycle/scalar)
     :signals (fx.prop/make (replace-from-map-mutator
                              (fn [^WebView view m]
                                (when-not (empty? m)
                                  (let [e (.getEngine view)]
                                    (on-loaded
                                      (.getLoadWorker e)
                                      (.executeScript (.getEngine view) (set-signal-html m))))))
                              nil)
                            fx.lifecycle/scalar)}))

(def ^:private default-config
  (delay
    {:background @style/background-color
     :font (str (.getFamily (font/font)) ", monospace")
     :title {:color @style/unfocused-selection-color
             :subtitleColor @style/unfocused-selection-color}
     :style {:guide-label {:fill (style/color :symbol)}
             :guide-title {:fill (style/color :symbol)}
             :group-title {:fill (style/color :symbol)}
             :cell {:stroke @style/unfocused-selection-color}}
     :axis {:domainColor (style/color :util)
            :gridColor @style/unfocused-selection-color
            :tickColor (style/color :util)}
     :range {:category ["#4285F4"
                        "#DB4437"
                        "#F4B400"
                        "#0F9D58"
                        "#AB47BC"
                        "#00ACC1"
                        "#FF7043"
                        "#9E9D24"
                        "#5C6BC0"
                        "#F06292"
                        "#00796B"
                        "#C2185B"]
             :heatmap {:scheme (case @style/theme :dark "darkmulti" :light "lightmulti")}}}))

(defn- deep-merge [& maps]
  (apply merge-with deep-merge maps))

(defn view [{:keys [spec opt data signals]}]
  (let [spec (if (and (map? spec)
                      (str/includes? (:$schema spec "https://vega.github.io/schema/vega-lite/v5.json")
                                     "vega-lite"))
               (-> spec
                   (update :width #(or % "container"))
                   (update :height #(or % "container")))
               spec)
        opt (-> opt
                (assoc :bind "#bind")
                (update :actions #(if (some? %) % false))
                (update :config #(deep-merge @default-config %)))]
    {:fx/type ext-with-data-props
     :props {:data (cond
                     (map? data) data
                     (coll? data) {"source" data})
             :signals signals}
     :desc {:fx/type fx.ext.web-view/with-engine-props
            :props {:content (html spec opt)
                    :on-create-popup create-popup}
            :desc {:fx/type :web-view}}}))