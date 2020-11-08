(ns vlaaad.reveal.style
  (:require [cljfx.css :as css]
            [vlaaad.reveal.font :as font]
            [vlaaad.reveal.prefs :as prefs]
            [clojure.edn :as edn]))

(def ^:private theme (delay (:theme @prefs/prefs :dark)))

(def colormap-base {:light {:scroll-bar-color                    "#0006"
                            :scroll-bar-background-color         "#eee3"
                            :scroll-bar-background-pressed-color "#fff6"
                            :inactive-scroll-bar-color           "#3333"
                            :selection-color                     "#64e3fc"
                            :unfocused-selection-color           "#e3e3e3"
                            :popup-color                         "#eee"
                            :search-color                        "#32383d"
                            :util                                "#999"
                            :symbol                              "#424b51"
                            :object                              "#c46003"
                            :string                              "018480"
                            :error                               "#f15856"
                            :success                             "#2a40ea"
                            :scalar                              "#2a40ea"
                            :keyword                             "#972aea"
                            :unfocused-background-color          "#f2f2f2"
                            :background-color                    "#fafafa"
                            }
                    :dark {:scroll-bar-color                     "#fff6"
                           :inactive-scroll-bar-color            "#eee3"
                           :scroll-bar-background-color          "#eee3"
                           :scroll-bar-background-pressed-color  "#fff6"
                           :selection-color                      "#005dd1"
                           :unfocused-selection-color            "#555"
                           :popup-color                          "#3b3b44"
                           :search-color                         "#aec1d0"
                           :util                                 "#888"
                           :symbol                               "#aec1d0"
                           :object                               "#f7b940"
                           :string                               "#22aeaa"
                           :error                                "#f15856"
                           :success                              "#64aa22"
                           :scalar                               "#649fe9"
                           :keyword                              "#ab6cf7"
                           :unfocused-background-color           "#333"
                           :background-color                     "#232325"
                           }})


;; helper from http://dnaeon.github.io/recursively-merging-maps-in-clojure/
(defn deep-merge
  "Recursively merges maps."
  [& maps]
  (letfn [(m [& xs]
            (if (some #(and (map? %) (not (record? %))) xs)
              (apply merge-with m xs)
              (last xs)))]
    (reduce m maps)))

(def colormap (let [colors (or (some-> (:color-scheme-file @prefs/prefs)
                                        slurp
                                        clojure.edn/read-string)
                               {})]
                (deep-merge colormap-base colors)))

(def scroll-bar-color (delay (get-in colormap [@theme :scroll-bar-color])))
(def inactive-scroll-bar-color (delay (get-in colormap [@theme :inactive-scroll-bar-color])))
(def selection-color (delay (get-in colormap [@theme :selection-color])))
(def unfocused-selection-color (delay (get-in colormap [@theme :unfocused-selection-color])))
(def popup-color (delay (get-in colormap [@theme :popup-color])))
(def search-color (delay (get-in colormap [@theme :search-color])))
(def search-shade-color (delay (get-in colormap [@theme :search-shade-color])))
(def default-padding 5)

(def ^:private colors
  (delay
    {:util (get-in colormap [@theme :util])
     :symbol (get-in colormap [@theme :symbol])
     :object (get-in colormap [@theme :object])
     :string (get-in colormap [@theme :string])
     :error   (get-in colormap [@theme :error])
     :success (get-in colormap [@theme :success])
     :scalar (get-in colormap [@theme :scalar])
     :keyword (get-in colormap [@theme :keyword])}))

(defn color [x]
  (get @colors x x))

(def style
  (let [util-color (color :util)
        symbol-color (color :symbol)
        unfocused-background-color (get-in colormap [@theme  :unfocused-background-color])
        background-color (get-in colormap [@theme  :background-color])
        scroll-bar-size 10
        font-family (str \" (.getFamily (font/font)) \")
        font-size (.getSize (font/font))
        smaller-font-size (int (* font-size 0.85))
        selection-color @selection-color
        unfocused-selection-color @unfocused-selection-color
        popup-color @popup-color
        scroll-bar-background-color (color :scroll-bar-background-color)
        scroll-bar-background-pressed-color (color :scroll-bar-background-pressed-color)
        ]
    (delay
      (css/register ::main
                    {".reveal"
                     {"-ui" {:-fx-background-color background-color}
                      "-popup" {:-fx-background-color popup-color
                                :-fx-spacing default-padding
                                :-fx-padding default-padding
                                "-scroll-pane" {:-fx-hbar-policy :never
                                                :-fx-min-height 0}
                                "-item" {:-fx-font-family font-family
                                         :-fx-text-fill util-color
                                         :-fx-max-width "Infinity"
                                         :-fx-padding [3.5 7]
                                         :-fx-font-size font-size
                                         ":hover" {:-fx-text-fill symbol-color}
                                         "-selected" {:-fx-background-color selection-color
                                                      :-fx-text-fill symbol-color}}}
                      "-text-field" {:-fx-font-family font-family
                                     :-fx-text-fill symbol-color
                                     :-fx-font-size font-size
                                     :-fx-background-color background-color
                                     :-fx-highlight-fill selection-color
                                     :-fx-highlight-text-fill symbol-color
                                     :-fx-padding [7 7]
                                     ":focused" {:-fx-border-color selection-color
                                                 :-fx-border-width [0 0 2 0]
                                                 :-fx-padding [7 7 5 7]}}
                      "-search" {:-fx-background-color popup-color
                                 :-fx-padding default-padding}
                      "-summary" {:-fx-font-family font-family
                                  :-fx-font-size font-size}
                      "-table"
                      {:-fx-table-cell-border-color unfocused-selection-color
                       :-fx-border-width [0 0 2 0]
                       :-fx-border-color :transparent
                       " .arrow" {:-fx-background-color util-color
                                  :-fx-padding [3 4 3 4]
                                  :-fx-shape "\"M 0 0 h 7 l -3.5 4 z\""}
                       " .filler" {:-fx-background-color unfocused-background-color}
                       "-column" {:-fx-padding 0
                                  :-fx-background-color unfocused-background-color
                                  :-fx-border-color [:transparent unfocused-selection-color :transparent :transparent]}
                       "-cell" {:-fx-border-color [:transparent unfocused-selection-color :transparent :transparent]
                                :-fx-padding 0
                                ":selected" {:-fx-background-color unfocused-selection-color}}
                       ":focused" {:-fx-border-color selection-color
                                   " .reveal-table-cell:selected" {:-fx-background-color selection-color}}}
                      "-chart"
                      {:-fx-focus-traversable true
                       :-fx-padding 2
                       ":focused" {:-fx-border-color selection-color
                                   :-fx-border-width [0 0 2 0]
                                   :-fx-padding [2 2 0 2]}
                       " .chart-pie-label" {:-fx-fill symbol-color
                                            :-fx-font-family font-family
                                            :-fx-font-size smaller-font-size}
                       " .axis" {:AXIS_COLOR unfocused-selection-color
                                 :-fx-tick-label-fill util-color
                                 "> Text" {:-fx-fill symbol-color
                                           :-fx-font-family font-family
                                           :-fx-font-size smaller-font-size}}
                       " .chart-vertical-grid-lines" {:-fx-stroke unfocused-selection-color}
                       " .chart-horizontal-grid-lines" {:-fx-stroke unfocused-selection-color}
                       " .chart-plot-background" {:-fx-background-color unfocused-background-color}
                       " .chart-content" {:-fx-padding 8}
                       " .chart-legend" {:-fx-background-color unfocused-background-color}}
                      "-view" {"-header" {:-fx-background-color popup-color
                                          "-tab" {"-focused" {:-fx-background-color background-color}}
                                          "-separator" {:-fx-min-width 1
                                                        :-fx-background-color background-color}}
                               "-scroll-pane" {:-fx-min-height 27}}}
                     ".table-row-cell" {:-fx-padding 0
                                        :-fx-cell-size (font/line-height)}
                     ".label" {:-fx-text-fill symbol-color
                               :-fx-font-family font-family
                               :-fx-font-size font-size
                               ":has-popup:focused" {:-fx-border-width [0 0 2 0]
                                                     :-fx-padding [0 0 -2 0]
                                                     :-fx-border-color selection-color}}
                     ".button" {:-fx-text-fill util-color
                                :-fx-font-family font-family
                                :-fx-font-size font-size
                                :-fx-background-color :transparent
                                :-fx-background-radius 0
                                :-fx-border-width 1
                                :-fx-border-color unfocused-selection-color
                                ":hover" {:-fx-text-fill symbol-color}
                                ":focused" {:-fx-background-color selection-color
                                            :-fx-text-fill symbol-color
                                            :-fx-border-color :transparent}}
                     ".cell" {:-fx-text-fill symbol-color
                              :-fx-background-color :transparent}
                     ;; scroll bars
                     ".table-cell" {:-fx-background-color :transparent}
                     ".scroll-pane" {:-fx-background-color :transparent
                                     "> .viewport" {:-fx-background-color :transparent}
                                     "> .corner" {:-fx-background-color :transparent}}
                     ".virtual-flow > .corner" {:-fx-background-color :transparent}
                     ".scroll-bar" {:-fx-background-color :transparent
                                    "> .thumb" {:-fx-background-color scroll-bar-background-color
                                                :-fx-background-insets 0
                                                :-fx-background-radius scroll-bar-size
                                                ":pressed" {:-fx-background-color scroll-bar-background-pressed-color}}
                                    ":horizontal" {"> .increment-button > .increment-arrow" {:-fx-pref-height scroll-bar-size}
                                                   "> .decrement-button > .decrement-arrow" {:-fx-pref-height scroll-bar-size}}
                                    ":vertical" {"> .increment-button > .increment-arrow" {:-fx-pref-width scroll-bar-size}
                                                 "> .decrement-button > .decrement-arrow" {:-fx-pref-width scroll-bar-size}}
                                    "> .decrement-button" {:-fx-padding 0
                                                           "> .decrement-arrow" {:-fx-shape nil
                                                                                 :-fx-padding 0}}
                                    "> .increment-button" {:-fx-padding 0
                                                           "> .increment-arrow" {:-fx-shape nil
                                                                                 :-fx-padding 0}}}}))))
