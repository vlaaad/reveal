(ns vlaaad.reveal.style
  (:require [cljfx.css :as css]
            [vlaaad.reveal.font :as font]
            [vlaaad.reveal.prefs :as prefs]))

(def theme (delay (:theme @prefs/prefs :dark)))

(def scroll-bar-color (delay (case @theme :dark "#fff6" :light "#0006")))
(def inactive-scroll-bar-color (delay (case @theme :dark "#eee3" :light "#3333")))
(def selection-color (delay (case @theme :dark "#004091" :light "#64e3fc")))
(def unfocused-selection-color (delay (case @theme :dark "#444444" :light "#e3e3e3")))
(def popup-color (delay (case @theme :dark "#3b3b44" :light "#eeeeee")))
(def search-color (delay (case @theme :dark "#aec1d0" :light "#32383d")))
(def background-color (delay (case @theme :dark "#232325" :light "#fafafa")))
(def search-shade-color (delay (str @search-color "55")))
(def default-padding 5)

(def ^:private colors
  (delay
    {:util (case @theme :dark "#888888" :light "#999999")
     :symbol (case @theme :dark "#aec1d0" :light "#424b51")
     :object (case @theme :dark "#f7b940" :light "#c46003")
     :string (case @theme :dark "#22aeaa" :light "#018480")
     :error "#f15856"
     :success (case @theme :dark "#64aa22" :light "#499304")
     :scalar (case @theme :dark "#649fe9" :light "#2a40ea")
     :keyword (case @theme :dark "#ab6cf7" :light "#972aea")}))

(defn color [x]
  (get @colors x x))

(def style
  (delay
    (let [util-color (color :util)
          symbol-color (color :symbol)
          object-color (color :object)
          scalar-color (color :scalar)
          error-color (color :error)
          success-color (color :success)
          unfocused-background-color (case @theme :dark "#333" :light "#f2f2f2")
          background-color @background-color
          scroll-bar-size 10
          font-family (str \" (.getFamily (font/font)) \")
          font-size (.getSize (font/font))
          smaller-font-size (int (* font-size 0.85))
          selection-color @selection-color
          unfocused-selection-color @unfocused-selection-color
          popup-color @popup-color
          char-width (font/char-width)
          scroll-bar-color @scroll-bar-color
          inactive-scroll-bar-color @inactive-scroll-bar-color]
      (css/register ::main
        {".reveal"
           {"-undecorated" {"-wrapper" {:-fx-background-color popup-color
                                        :-fx-padding 1}
                            "-resize" {:-fx-min-width 12
                                       :-fx-min-height 12
                                       :-fx-max-width 12
                                       :-fx-max-height 12
                                       :-fx-background-color util-color
                                       :-fx-shape "\"M4 0 L5 0 L6 1 L6 2z M2 0 L3 0 L6 3 L6 4z M0 0 L1 0 L6 5 L6 6z\""
                                       :-fx-cursor :ne-resize}
                            "-title" {:-fx-font-family font-family
                                      :-fx-text-fill symbol-color
                                      :-fx-font-size smaller-font-size
                                      ":draggable" {:-fx-cursor :move}}}
            "-ui" {:-fx-background-color background-color}
            "-popup" {:-fx-background-color popup-color
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
               "-column" {:-fx-padding -1
                          :-fx-background-color unfocused-background-color
                          :-fx-border-color [:transparent unfocused-selection-color :transparent :transparent]
                          " .label" {:-fx-padding [0 2]
                                     :-fx-alignment :top-left}}
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
               " .chart-legend" {:-fx-background-color unfocused-background-color}
               " .chart-line-symbol" {:-fx-background-insets "0, 1"
                                      :-fx-padding 2}
               " .chart-symbol" {:-fx-padding 3}
               " .chart-series-line" {:-fx-stroke-width 2}}
            "-test" {"-tree-cell" {:-fx-font-size font-size
                                   :-fx-font-family font-family
                                   ":pass" {:-fx-text-fill success-color}
                                   ":fail" {:-fx-text-fill error-color}
                                   ":out" {:-fx-text-fill util-color}}
                     "-time-label" {:-fx-font-size font-size
                                    :-fx-font-family font-family
                                    :-fx-text-fill util-color}
                     "-open-view-button" {:-fx-max-width "Infinity"
                                          :-fx-border-insets [0 0 0 -1]}
                     "-label" {:-fx-font-family font-family
                               :-fx-font-size font-size
                               :-fx-min-width 50
                               ":default" {:-fx-text-fill symbol-color
                                           ">.reveal-test-icon" {:-fx-background-color symbol-color}}
                               ":success" {:-fx-text-fill success-color
                                           ">.reveal-test-icon" {:-fx-background-color success-color}}
                               ":error" {:-fx-text-fill error-color
                                         ">.reveal-test-icon" {:-fx-background-color error-color}}}
                     "-icon" {:-fx-background-color symbol-color
                              ":running" {:-fx-shape "\"M3.254,6.572c0.008,0.072,0.048,0.123,0.082,0.187c0.036,0.07,0.06,0.137,0.12,0.187C3.47,6.957,3.47,6.978,3.484,6.988c0.048,0.034,0.108,0.018,0.162,0.035c0.057,0.019,0.1,0.066,0.164,0.066c0.004,0,0.01,0,0.015,0l2.934-0.074c0.317-0.007,0.568-0.271,0.56-0.589C7.311,6.113,7.055,5.865,6.744,5.865c-0.005,0-0.01,0-0.015,0L5.074,5.907c2.146-2.118,5.604-2.634,7.971-1.007c2.775,1.912,3.48,5.726,1.57,8.501c-1.912,2.781-5.729,3.486-8.507,1.572c-0.259-0.18-0.618-0.119-0.799,0.146c-0.18,0.262-0.114,0.621,0.148,0.801c1.254,0.863,2.687,1.279,4.106,1.279c2.313,0,4.591-1.1,6.001-3.146c2.268-3.297,1.432-7.829-1.867-10.101c-2.781-1.913-6.816-1.36-9.351,1.058L4.309,3.567C4.303,3.252,4.036,3.069,3.72,3.007C3.402,3.015,3.151,3.279,3.16,3.597l0.075,2.932C3.234,6.547,3.251,6.556,3.254,6.572z\""
                                          :-fx-pref-width 10
                                          :-fx-pref-height 10}
                              ":pass" {:-fx-shape "\"M7.629,14.566c0.125,0.125,0.291,0.188,0.456,0.188c0.164,0,0.329-0.062,0.456-0.188l8.219-8.221c0.252-0.252,0.252-0.659,0-0.911c-0.252-0.252-0.659-0.252-0.911,0l-7.764,7.763L4.152,9.267c-0.252-0.251-0.66-0.251-0.911,0c-0.252,0.252-0.252,0.66,0,0.911L7.629,14.566z\""
                                       :-fx-pref-width 10
                                       :-fx-pref-height 7}
                              ":fail" {:-fx-shape "\"M15.898,4.045c-0.271-0.272-0.713-0.272-0.986,0l-4.71,4.711L5.493,4.045c-0.272-0.272-0.714-0.272-0.986,0s-0.272,0.714,0,0.986l4.709,4.711l-4.71,4.711c-0.272,0.271-0.272,0.713,0,0.986c0.136,0.136,0.314,0.203,0.492,0.203c0.179,0,0.357-0.067,0.493-0.203l4.711-4.711l4.71,4.711c0.137,0.136,0.314,0.203,0.494,0.203c0.178,0,0.355-0.067,0.492-0.203c0.273-0.273,0.273-0.715,0-0.986l-4.711-4.711l4.711-4.711C16.172,4.759,16.172,4.317,15.898,4.045z\""
                                       :-fx-pref-width 10
                                       :-fx-pref-height 10
                                       :-fx-background-insets [1 1]}
                              ":test" {:-fx-shape "\"M2.25,12.584c-0.713,0-1.292,0.578-1.292,1.291s0.579,1.291,1.292,1.291c0.713,0,1.292-0.578,1.292-1.291S2.963,12.584,2.25,12.584z M2.25,14.307c-0.238,0-0.43-0.193-0.43-0.432s0.192-0.432,0.43-0.432c0.238,0,0.431,0.193,0.431,0.432S2.488,14.307,2.25,14.307z M5.694,6.555H18.61c0.237,0,0.431-0.191,0.431-0.43s-0.193-0.431-0.431-0.431H5.694c-0.238,0-0.43,0.192-0.43,0.431S5.457,6.555,5.694,6.555z M2.25,8.708c-0.713,0-1.292,0.578-1.292,1.291c0,0.715,0.579,1.292,1.292,1.292c0.713,0,1.292-0.577,1.292-1.292C3.542,9.287,2.963,8.708,2.25,8.708z M2.25,10.43c-0.238,0-0.43-0.192-0.43-0.431c0-0.237,0.192-0.43,0.43-0.43c0.238,0,0.431,0.192,0.431,0.43C2.681,10.238,2.488,10.43,2.25,10.43z M18.61,9.57H5.694c-0.238,0-0.43,0.192-0.43,0.43c0,0.238,0.192,0.431,0.43,0.431H18.61c0.237,0,0.431-0.192,0.431-0.431C19.041,9.762,18.848,9.57,18.61,9.57z M18.61,13.443H5.694c-0.238,0-0.43,0.193-0.43,0.432s0.192,0.432,0.43,0.432H18.61c0.237,0,0.431-0.193,0.431-0.432S18.848,13.443,18.61,13.443z M2.25,4.833c-0.713,0-1.292,0.578-1.292,1.292c0,0.713,0.579,1.291,1.292,1.291c0.713,0,1.292-0.578,1.292-1.291C3.542,5.412,2.963,4.833,2.25,4.833z M2.25,6.555c-0.238,0-0.43-0.191-0.43-0.43s0.192-0.431,0.43-0.431c0.238,0,0.431,0.192,0.431,0.431S2.488,6.555,2.25,6.555z\""
                                       :-fx-pref-width 10
                                       :-fx-pref-height 10}
                              ":start" {:-fx-pref-width 10
                                        :-fx-pref-height 10
                                        :-fx-shape "\"M0 0 L1 1 L0 2z\""
                                        :-fx-background-insets [-2 0]}}}
            "-view" {"-header" {:-fx-background-color popup-color
                                "-button" {:-fx-text-fill symbol-color
                                           :-fx-font-family font-family
                                           :-fx-font-size font-size
                                           :-fx-focus-traversable false
                                           :-fx-padding [3 7]
                                           ":hover" {:-fx-background-color unfocused-selection-color}
                                           ":disabled" {:-fx-text-fill unfocused-selection-color}}
                                "-separator" {:-fx-min-width 1
                                              :-fx-background-color background-color}}
                     "-result-tree" {:-fx-background-color popup-color
                                     "-item" {:-fx-padding [3.5 7]
                                              ":selected" {:-fx-background-color selection-color}}}}}
         ".table-row-cell" {:-fx-padding 0
                            :-fx-cell-size (font/line-height)}
         ".label" {:-fx-text-fill symbol-color
                   :-fx-font-family font-family
                   :-fx-font-size font-size
                   ":has-popup:focused" {:-fx-border-width [0 0 2 0]
                                         :-fx-padding [0 0 -2 0]
                                         :-fx-border-color selection-color}}
         ".text-field" {:-fx-font-family font-family
                        :-fx-text-fill symbol-color
                        :-fx-font-size font-size
                        :-fx-background-color background-color
                        :-fx-highlight-fill selection-color
                        :-fx-highlight-text-fill symbol-color
                        :-fx-prompt-text-fill util-color
                        :-fx-background-radius 0
                        :-fx-padding [7 7]
                        ":focused" {:-fx-border-color selection-color
                                    :-fx-border-width [0 0 2 0]
                                    :-fx-padding [7 7 5 7]}}
         ".hyperlink" {:-fx-font-family font-family
                       :-fx-text-fill scalar-color
                       :-fx-padding 0
                       :-fx-font-size font-size
                       ":focused" {:-fx-border-color scalar-color}}
         ".button" {:-fx-text-fill symbol-color
                    :-fx-font-family font-family
                    :-fx-font-size font-size
                    :-fx-background-color :transparent
                    :-fx-background-radius 0
                    :-fx-border-width 1
                    :-fx-border-color unfocused-selection-color
                    ":hover" {:-fx-background-color unfocused-selection-color
                              :-fx-background-insets 0
                              ":focused" {:-fx-background-color selection-color}}
                    ":focused" {:-fx-background-color selection-color
                                :-fx-text-fill symbol-color
                                :-fx-border-color :transparent}}
         ".tooltip" {:-fx-background-color unfocused-background-color
                     :-fx-font-family font-family
                     :-fx-font-size font-size
                     :-fx-text-fill symbol-color
                     :-fx-padding [3.5 7]
                     :-fx-background-radius 0}
         ".cell" {:-fx-text-fill symbol-color
                  :-fx-background-color :transparent}
         ".menu-item" {:-fx-padding [3.5 7]
                       "> .label" {:-fx-text-fill util-color}
                       ":focused" {:-fx-background-color selection-color
                                   "> .label" {:-fx-text-fill symbol-color}}}
         ".context-menu" {:-fx-background-color popup-color
                          :-fx-padding default-padding
                          :-fx-effect "dropshadow(gaussian, #0006, 10, 0, 0, 5)"
                          " .separator > .line" {:-fx-border-insets [3.5 -7]}}
         ".separator" {"> .line" {:-fx-border-color [background-color :transparent :transparent :transparent]}}
         ".tree-view" {:-fx-background-color :transparent
                       :-fx-padding 0
                       ":focused > .virtual-flow > .clipped-container > .sheet > .tree-cell"
                       {":filled:selected" {:-fx-background-color selection-color}}
                       "> .virtual-flow > .clipped-container > .sheet > .tree-cell"
                       {":filled:selected" {:-fx-background-color unfocused-selection-color}}}
         ".tree-cell" {:-fx-text-fill util-color
                       :-fx-graphic-text-gap char-width
                       :-fx-font-size font-size
                       :-fx-font-family font-family
                       :-fx-padding 1
                       ":enabled" {:-fx-text-fill symbol-color}
                       "> .tree-disclosure-node > .arrow" {:-fx-background-color util-color}}
         ;; scroll bars
         ".table-cell" {:-fx-background-color :transparent}
         ".scroll-pane" {:-fx-background-color :transparent
                         :-fx-padding 0
                         "> .viewport" {:-fx-background-color :transparent}
                         "> .corner" {:-fx-background-color :transparent}}
         ".virtual-flow > .corner" {:-fx-background-color :transparent}
         ".split-pane" {:-fx-background-color :transparent
                        ">.split-pane-divider"
                        {:-fx-background-color :transparent
                         ":hover" {">.vertical-grabber" {:-fx-background-color scroll-bar-color}
                                   ">.horizontal-grabber" {:-fx-background-color scroll-bar-color}}
                         ">.vertical-grabber" {:-fx-pref-width 50
                                               :-fx-pref-height 3
                                               :-fx-background-radius 3
                                               :-fx-background-color inactive-scroll-bar-color}
                         ">.horizontal-grabber" {:-fx-pref-width 3
                                                 :-fx-pref-height 50
                                                 :-fx-background-radius 3
                                                 :-fx-background-color inactive-scroll-bar-color}}}
         ".scroll-bar" {:-fx-background-color :transparent
                        "> .thumb" {:-fx-background-color inactive-scroll-bar-color
                                    :-fx-background-insets 0
                                    :-fx-background-radius scroll-bar-size
                                    ":pressed" {:-fx-background-color scroll-bar-color}}
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