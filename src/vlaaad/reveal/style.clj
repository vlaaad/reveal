(ns vlaaad.reveal.style
  (:require [cljfx.css :as css]
            [vlaaad.reveal.font :as font]))

(def scroll-bar-size 10)
(def util-color "#888")
(def symbol-color "#aec1d0")
(def selection-color "#005dd1")
(def unfocused-selection-color "#555")
(def unfocused-background-color "#333")
(def popup-color "#3b3b44")
(def object-color "#f7b940")
(def background-color "#232325")
(def font-family (str \" (.getFamily font/font) \"))
(def font-size (.getSize font/font))
(def smaller-font-size (int (* font-size 0.85)))
(def string-color "#22aeaa")
(def error-color "#f15856")
(def success-color "#64aa22")
(def scalar-color "#649fe9")
(def keyword-color "#a55cfc")
(def search-color "#aec1d0")
(def search-shade-color (str search-color "55"))
(def default-padding 5)

(def style
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
                   :-fx-padding 5}
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
                        :-fx-cell-size font/line-height}
     ".label" {:-fx-text-fill symbol-color
               :-fx-font-family font-family
               :-fx-font-size font-size}
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
     ".virtual-flow > .corner" {:-fx-background-color :transparent}
     ".scroll-bar" {:-fx-background-color :transparent
                    "> .thumb" {:-fx-background-color "#eee3"
                                :-fx-background-insets 0
                                :-fx-background-radius scroll-bar-size
                                ":pressed" {:-fx-background-color "#fff6"}}
                    ":horizontal" {"> .increment-button > .increment-arrow" {:-fx-pref-height scroll-bar-size}
                                   "> .decrement-button > .decrement-arrow" {:-fx-pref-height scroll-bar-size}}
                    ":vertical" {"> .increment-button > .increment-arrow" {:-fx-pref-width scroll-bar-size}
                                 "> .decrement-button > .decrement-arrow" {:-fx-pref-width scroll-bar-size}}
                    "> .decrement-button" {:-fx-padding 0
                                           "> .decrement-arrow" {:-fx-shape nil
                                                                 :-fx-padding 0}}
                    "> .increment-button" {:-fx-padding 0
                                           "> .increment-arrow" {:-fx-shape nil
                                                                 :-fx-padding 0}}}}))