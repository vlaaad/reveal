(ns vlaaad.reveal.style
  (:require [cljfx.css :as css]
            [vlaaad.reveal.font :as font]))

(def style
  (css/register ::main
    (let [cell-height 25
          scroll-bar-size 10
          util-color "#888"
          symbol-color "#aec1d0"
          selection-color "#005dd1"
          unfocused-selection-color "#555"
          popup-color "#3b3b44"
          object-color "#f7b940"
          background-color "#232325"
          font-family (str \" (.getFamily font/font) \")
          font-size (.getSize font/font)]
      {::util-color util-color
       ::string-color "#22aeaa"
       ::error-color "#f15856"
       ::scalar-color "#649fe9"
       ::keyword-color "#a55cfc"
       ::symbol-color symbol-color
       ::object-color object-color
       ::background-color background-color
       ::selection-color selection-color
       ::unfocused-selection-color unfocused-selection-color
       ::popup-color popup-color
       ::scroll-bar-size scroll-bar-size
       ::cell-height cell-height
       ".reveal" {"-ui" {:-fx-background-color background-color}
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
                  "-view" {"-header" {:-fx-background-color popup-color
                                      "-tab" {"-focused" {:-fx-background-color background-color}}
                                      "-separator" {:-fx-min-width 1
                                                    :-fx-background-color background-color}}
                           "-scroll-pane" {:-fx-min-height 27}}}
       ".label" {:-fx-text-fill util-color
                 :-fx-font-family font-family
                 :-fx-font-size font-size}
       ".cell" {:-fx-text-fill symbol-color
                :-fx-background-color :transparent
                ":focused" {:-fx-background-color selection-color}}
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
                                                                   :-fx-padding 0}}}})))