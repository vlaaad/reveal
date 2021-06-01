(ns e08-vega-view
  (:require [cljfx.ext.web-view :as fx.ext.web-view]
            [clojure.data.json :as json]
            [vlaaad.reveal.ext :as rx]))

;; Define vega view

(defn vega-view [{:keys [spec]}]
  {:fx/type fx.ext.web-view/with-engine-props
   :props {:content (str "<head>
                            <script src=\"https://cdn.jsdelivr.net/npm/vega@5\"></script>
                            <script src=\"https://cdn.jsdelivr.net/npm/vega-lite@4\"></script>
                            <script src=\"https://cdn.jsdelivr.net/npm/vega-embed@6\"></script>
                          </head>
                          <body>
                            <div id=\"view\"></div>
                            <script>vegaEmbed('#view', " (json/write-str spec) ");</script>
                          </body>")}
   :desc {:fx/type :web-view}})

;; Use at the REPL with Reveal commands to view vega charts without leaving the editor

(rx/all
  (rx/close-all-views)
  (rx/open-view
    {:fx/type vega-view
     :spec {:data {:url "https://vega.github.io/vega-lite/data/seattle-weather.csv"}
            :mark :bar
            :encoding {:x {:timeUnit :month
                           :field :date
                           :type :ordinal}
                       :y {:aggregate :mean
                           :field :precipitation}}}}))
