(ns vlaaad.reveal.popup
  (:require [cljfx.lifecycle :as fx.lifecycle]
            [cljfx.composite :as fx.composite]
            [cljfx.fx.popup :as fx.popup]
            [cljfx.mutator :as fx.mutator]
            [vlaaad.reveal.style :as style]
            [cljfx.fx.drop-shadow :as fx.drop-shadow]
            [cljfx.fx.polygon :as fx.polygon]
            [cljfx.fx.v-box :as fx.v-box]
            [cljfx.fx.stack-pane :as fx.stack-pane])
  (:import [javafx.stage Popup Window Screen]
           [java.util Collection]
           [javafx.geometry Bounds Rectangle2D]
           [javafx.event Event]))

(defn consume-popup-event [^Event e]
  (.consume e))

(def lifecycle
  (fx.lifecycle/wrap-on-delete
    (fx.composite/describe Popup
      :ctor []
      :props (merge fx.popup/props
                    (fx.composite/props Popup
                      :window [(fx.mutator/setter
                                 (fn [^Popup popup ^Window window]
                                   (if window
                                     (.show popup window)
                                     (.hide popup))))
                               fx.lifecycle/scalar]
                      :stylesheets [(fx.mutator/setter
                                      (fn [^Popup popup ^Collection styles]
                                        (.setAll (.getStylesheets (.getScene popup)) styles)))
                                    fx.lifecycle/scalar
                                    :default []])))
    #(.hide ^Popup %)))

(defn more-screen-space-below? [bounds]
  (let [^Screen screen (or (first (Screen/getScreensForRectangle (.getMinX bounds)
                                                                 (.getMinY bounds)
                                                                 (.getWidth bounds)
                                                                 (.getHeight bounds)))
                           (Screen/getPrimary))
        screen-bounds (.getVisualBounds screen)
        bounds-min-x (max (.getMinX screen-bounds) (.getMinX bounds))
        bounds-min-y (max (.getMinY screen-bounds) (.getMinY bounds))
        bounds (Rectangle2D.
                 bounds-min-x
                 bounds-min-y
                 (max 1 (- (min (.getMaxX screen-bounds) (.getMaxX bounds)) bounds-min-x))
                 (max 1 (- (min (.getMaxY screen-bounds) (.getMaxY bounds)) bounds-min-y)))
        space-below (- (.getMaxY screen-bounds) (.getMaxY bounds))
        space-above (- (.getMinY bounds) (.getMinY screen-bounds))]
    (< space-above space-below)))

(defn view
  "Cljfx popup view

   Required args:
   - :bounds - screen bounds for popup target
   - :window - target window
   - :on-cancel - event map
   - :position - :top or :bottom
   - :desc - content description

   Optional:
   - :alignment - :center (default) or :left
   - :width (default 300)
   - :event-handler (default consumes all events)

   All other props are forwarded to :popup component description"
  [{:keys [^Bounds bounds
           on-cancel
           position
           alignment
           event-handler
           desc
           width]
    :or {width 300
         alignment :center
         event-handler consume-popup-event}
    :as props}]
  (let [^Screen screen (or (first (Screen/getScreensForRectangle (.getMinX bounds)
                                                                 (.getMinY bounds)
                                                                 (.getWidth bounds)
                                                                 (.getHeight bounds)))
                           (Screen/getPrimary))
        screen-bounds (.getVisualBounds screen)
        bounds-min-x (max (.getMinX screen-bounds) (.getMinX bounds))
        bounds-min-y (max (.getMinY screen-bounds) (.getMinY bounds))
        bounds (Rectangle2D.
                 bounds-min-x
                 bounds-min-y
                 (max 1 (- (min (.getMaxX screen-bounds) (.getMaxX bounds)) bounds-min-x))
                 (max 1 (- (min (.getMaxY screen-bounds) (.getMaxY bounds)) bounds-min-y)))
        shadow-radius 10
        shadow-offset-y 5
        popup-width (+ width shadow-radius shadow-radius)
        space-below (- (.getMaxY screen-bounds) (.getMaxY bounds))
        space-above (- (.getMinY bounds) (.getMinY screen-bounds))
        popup-at-the-bottom (case position :bottom true :top false)
        pref-anchor-x (case alignment
                        :left (- (.getMinX bounds) shadow-radius)
                        :center (-> (.getMinX bounds)
                                    (+ (* (.getWidth bounds) 0.5))
                                    (- (* popup-width 0.5))))
        visible-start-x (+ pref-anchor-x shadow-radius)
        visible-end-x (+ pref-anchor-x popup-width (- shadow-radius))
        anchor-fix-x (cond
                       (< visible-start-x (.getMinX screen-bounds))
                       (- (.getMinX screen-bounds) visible-start-x)

                       (> visible-end-x (.getMaxX screen-bounds))
                       (- (.getMaxX screen-bounds) visible-end-x)

                       :else
                       0)
        arrow-width 10
        arrow-height 10
        arrow-x (case alignment
                  :center (- (* width 0.5) anchor-fix-x)
                  :left (- (* 0.5 shadow-radius) anchor-fix-x))
        max-content-height (- (if popup-at-the-bottom space-below space-above)
                              arrow-height)]
    (into
      {:fx/type lifecycle
       :stylesheets [(:cljfx.css/url @style/style)]
       :anchor-location (if popup-at-the-bottom :window-top-left :window-bottom-left)
       :anchor-x (+ pref-anchor-x anchor-fix-x)
       :anchor-y (if popup-at-the-bottom
                   (- (.getMaxY bounds) shadow-radius (- shadow-offset-y))
                   (+ (.getMinY bounds) shadow-radius shadow-offset-y))
       :auto-fix false
       :hide-on-escape false
       :auto-hide true
       :on-auto-hide on-cancel
       :event-handler event-handler
       :content [{:fx/type fx.v-box/lifecycle
                  :pref-width width
                  :max-width width
                  :effect {:fx/type fx.drop-shadow/lifecycle
                           :radius shadow-radius
                           :offset-y shadow-offset-y
                           :color "#0006"}
                  :children (-> []
                                (cond-> popup-at-the-bottom
                                  (conj {:fx/type fx.polygon/lifecycle
                                         :v-box/margin {:left (- arrow-x (* arrow-width 0.5))}
                                         :fill @style/popup-color
                                         :points [0 arrow-height
                                                  arrow-width arrow-height
                                                  (* arrow-width 0.5) 0]}))
                                (conj
                                  {:fx/type fx.stack-pane/lifecycle
                                   :style-class "reveal-popup"
                                   :max-height max-content-height
                                   :children [desc]})
                                (cond-> (not popup-at-the-bottom)
                                  (conj {:fx/type fx.polygon/lifecycle
                                         :v-box/margin {:left (- arrow-x (* arrow-width 0.5))}
                                         :fill @style/popup-color
                                         :points [0 0
                                                  arrow-width 0
                                                  (* arrow-width 0.5) arrow-height]})))}]}
      (dissoc props :bounds :on-cancel :position :alignment :desc :width :event-handler))))