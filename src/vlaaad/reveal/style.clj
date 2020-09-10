(ns vlaaad.reveal.style
  (:require [cljfx.css :as css]
            [vlaaad.reveal.font :as font]
            [vlaaad.reveal.prefs :as prefs]))

(def ^:private theme (:theme prefs/prefs :dark))

(def scroll-bar-color (case theme :dark "#fff6" :light "#0006"))
(def inactive-scroll-bar-color (case theme :dark "#eee3" :light "#3333"))
(def util-color (case theme :dark "#888" :light "#999"))
(def symbol-color (case theme :dark "#aec1d0" :light "#424b51"))
(def selection-color (case theme :dark "#005dd1" :light "#64e3fc"))
(def unfocused-selection-color (case theme :dark "#555" :light "#e3e3e3"))
(def unfocused-background-color (case theme :dark "#333" :light "#f2f2f2"))
(def popup-color (case theme :dark "#3b3b44" :light "#eee"))
(def object-color (case theme :dark "#f7b940" :light "#c46003"))
(def background-color (case theme :dark "#232325" :light "#fafafa"))
(def string-color (case theme :dark "#22aeaa" :light "#018480"))
(def error-color "#f15856")
(def success-color (case theme :dark "#64aa22" :light "#499304"))
(def scalar-color (case theme :dark "#649fe9" :light "#2a40ea"))
(def keyword-color (case theme :dark "#ab6cf7" :light "#972aea"))
(def search-color (case theme :dark "#aec1d0" :light "#32383d"))
(def search-shade-color (str search-color "55"))
(def scroll-bar-size 10)
(def default-padding 5)
(def font-family (str \" (.getFamily font/font) \"))
(def font-size (.getSize font/font))
(def smaller-font-size (int (* font-size 0.85)))

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