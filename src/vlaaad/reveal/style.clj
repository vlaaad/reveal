(ns vlaaad.reveal.style
  (:require [cljfx.css :as css]
            [vlaaad.reveal.font :as font]))

(def cell-height 25)
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
(def string-color "#22aeaa")
(def error-color "#f15856")
(def scalar-color "#649fe9")
(def keyword-color "#a55cfc")

(def style
  (css/register ::main
    {".reveal"
       {"-ui" {:-fx-background-color background-color}
        "-popup" {:-fx-background-color popup-color
                  :-fx-spacing 5
                  :-fx-padding 5
                  "-scroll-pane" {:-fx-hbar-policy :never
                                  :-fx-min-height 0}
                  "-item" {:-fx-font-family font-family
                           :-fx-text-fill util-color
                           :-fx-max-width "Infinity"
                           :-fx-padding [3.5 7]
                           :-fx-font-size font-size
                           ":hover" {:-fx-text-fill symbol-color}
                           "-selected" {:-fx-background-color selection-color
                                        :-fx-text-fill symbol-color}}
                  "-text-field" {:-fx-font-family font-family
                                 :-fx-text-fill symbol-color
                                 :-fx-font-size font-size
                                 :-fx-background-color background-color
                                 :-fx-highlight-fill selection-color
                                 :-fx-highlight-text-fill symbol-color
                                 :-fx-padding [7 7]
                                 ":focused" {:-fx-border-color selection-color
                                             :-fx-border-width [0 0 2 0]
                                             :-fx-padding [7 7 5 7]}}}
        "-summary" {:-fx-font-family font-family
                    :-fx-font-size font-size}
        "-table"
          {" .arrow" {:-fx-background-color util-color
                      :-fx-padding [3 4 3 4]
                      :-fx-shape "\"M 0 0 h 7 l -3.5 4 z\""}
           " .filler" {:-fx-background-color unfocused-background-color}
           :-fx-table-cell-border-color unfocused-selection-color
           "-column" {:-fx-padding 0
                      :-fx-background-color unfocused-background-color
                      :-fx-border-color [:transparent unfocused-selection-color :transparent :transparent]}
           "-cell" {:-fx-border-color [:transparent unfocused-selection-color :transparent :transparent]
                    :-fx-padding 0
                    ":selected" {:-fx-background-color unfocused-selection-color}}
           ":focused .reveal-table-cell:selected" {:-fx-background-color selection-color}}
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