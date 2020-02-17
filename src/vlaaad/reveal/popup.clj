(ns vlaaad.reveal.popup
  (:require [vlaaad.reveal.event :as event]
            [cljfx.fx.popup :as fx.popup]
            [vlaaad.reveal.search :as search]
            [cljfx.composite :as fx.composite]
            [cljfx.lifecycle :as fx.lifecycle]
            [cljfx.mutator :as fx.mutator]
            [vlaaad.reveal.style :as style]
            [cljfx.api :as fx]
            [cljfx.ext.list-view :as fx.ext.list-view])
  (:import [javafx.geometry Bounds Rectangle2D]
           [javafx.stage Screen Popup]
           [com.sun.javafx.event RedirectedEvent]
           [javafx.event Event]
           [javafx.scene.input KeyCode KeyEvent]
           [java.util Collection]
           [javafx.scene.control ListView]))

(defn- consume-popup-event [^Event e]
  (if (instance? RedirectedEvent e)
    (.consume (.getOriginalEvent ^RedirectedEvent e))
    (.consume e)))

(defn- on-key-pressed [{:keys [state id ^KeyEvent fx/event on-cancel segments]}]
  (let [this (get-in state [id :popup])]
    (condp = (.getCode event)
      KeyCode/ESCAPE
      (if (empty? (:filter-text this))
        {:dispatch on-cancel}
        {:state (update-in state [id :popup] dissoc :filter-text)})

      KeyCode/ENTER
      (when-let [action (.getFocusedItem (.getFocusModel ^ListView (.getTarget event)))]
        {:execute (assoc action :segments segments)
         :dispatch on-cancel})
      nil)))

(defn- on-item-clicked [{:keys [action segments on-cancel]}]
  {:execute (assoc action :segments segments)
   :dispatch on-cancel})

(defn- on-result-key-pressed [{:keys [^KeyEvent fx/event on-cancel]}]
  (when (= KeyCode/ESCAPE (.getCode event))
    {:dispatch on-cancel}))

(defn- on-key-typed [{:keys [state id ^KeyEvent fx/event]}]
  (let [ch (.getCharacter event)
        this (get-in state [id :popup])
        ^String text (:filter-text this "")]
    (cond
      (.isEmpty ch)
      nil

      (= 27 (int (.charAt ch 0)))
      nil

      (= "\r" ch)
      nil

      (and (= "\b" ch) (not (.isEmpty text)))
      {:state (assoc-in state [id :popup :filter-text] (subs text 0 (dec (.length text))))}


      (= "\b" ch)
      nil

      :else
      {:state (assoc-in state [id :popup :filter-text] (str text ch))})))

(defn- on-selected-action-changed [{:keys [state id fx/event]}]
  {:state (assoc-in state [id :popup :selected-action] event)})

(defn- on-item-hover [{:keys [state id action]}]
  {:state (assoc-in state [id :popup :selected-action] action)})

(def ^:private lifecycle
  (fx.composite/describe Popup
    :ctor []
    :props (merge fx.popup/props
                  (fx.composite/props Popup
                    :stylesheets [(fx.mutator/setter
                                    (fn [^Popup popup ^Collection styles]
                                      (.setAll (.getStylesheets (.getScene popup)) styles)))
                                  fx.lifecycle/scalar
                                  :default []]))))

(defn view [{:keys [actions
                    ^Bounds bounds
                    id
                    on-cancel
                    filter-text
                    selected-action
                    output
                    segments]
             :or {filter-text ""
                  output ::no-output}}]
  (let [actions (cond-> actions
                        (not= "" filter-text)
                        (search/select filter-text :label))
        ^Screen screen (first (Screen/getScreensForRectangle (.getMinX bounds)
                                                             (.getMinY bounds)
                                                             (.getWidth bounds)
                                                             (.getHeight bounds)))
        screen-bounds (.getVisualBounds screen)
        bounds-min-x (max (.getMinX screen-bounds) (.getMinX bounds))
        bounds-min-y (max (.getMinY screen-bounds) (.getMinY bounds))
        bounds (Rectangle2D. bounds-min-x
                             bounds-min-y
                             (- (min (.getMaxX screen-bounds) (.getMaxX bounds))
                                bounds-min-x)
                             (- (min (.getMaxY screen-bounds) (.getMaxY bounds))
                                bounds-min-y))
        content-width 300
        shadow-radius 10
        shadow-offset-y 5
        popup-width (+ content-width shadow-radius shadow-radius)
        space-below (- (.getMaxY screen-bounds)
                       (.getMaxY bounds))
        space-above (- (.getMinY bounds)
                       (.getMinY screen-bounds))
        popup-at-the-bottom (< space-above space-below)
        pref-anchor-x (-> (.getMinX bounds)
                          (+ (* (.getWidth bounds) 0.5))
                          (- (* popup-width 0.5)))
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
        arrow-x (- (* content-width 0.5) anchor-fix-x)
        max-content-height (- (if popup-at-the-bottom
                                space-below
                                space-above)
                              arrow-height)]
    {:fx/type lifecycle
     :stylesheets [(:cljfx.css/url style/style)]
     :anchor-location (if popup-at-the-bottom :window-top-left :window-bottom-left)
     :anchor-x (+ pref-anchor-x anchor-fix-x)
     :anchor-y (if popup-at-the-bottom
                 (- (.getMaxY bounds) shadow-radius (- shadow-offset-y))
                 (+ (.getMinY bounds) shadow-radius shadow-offset-y))
     :auto-fix false
     :hide-on-escape false
     :auto-hide true
     :on-auto-hide on-cancel
     :event-handler consume-popup-event
     :content
     [{:fx/type :v-box
       :pref-width content-width
       :max-width content-width
       :effect {:fx/type :drop-shadow
                :radius shadow-radius
                :offset-y shadow-offset-y
                :color "#0006"}
       :children
       (-> []
           (cond-> popup-at-the-bottom
                   (conj {:fx/type :polygon
                          :v-box/margin {:left (- arrow-x (* arrow-width 0.5))}
                          :fill (::style/popup-color style/style)
                          :points [0 arrow-height
                                   arrow-width arrow-height
                                   (* arrow-width 0.5) 0]}))
           (conj
             (if (= output ::no-output)
               {:fx/type :stack-pane
                :style-class "reveal-popup"
                :children
                (cond-> [{:fx/type fx.ext.list-view/with-selection-props
                          :props {:selected-item (or (and selected-action
                                                          (some #(when (= % selected-action) %) actions))
                                                     (first actions))
                                  :on-selected-item-changed {::event/handler on-selected-action-changed :id id}}
                          :desc {:fx/type :list-view
                                 :style-class "reveal-popup-list-view"
                                 :placeholder {:fx/type :label
                                               :style-class "reveal-placeholder"
                                               :text "No actions match"}
                                 :pref-height (+ (::style/scroll-bar-size style/style)
                                                 1
                                                 (* (count actions)
                                                    (::style/cell-height style/style)))
                                 :on-key-typed {::event/handler on-key-typed :id id}
                                 :on-key-pressed {::event/handler on-key-pressed
                                                  :id id
                                                  :on-cancel on-cancel
                                                  :segments segments}
                                 :max-height max-content-height
                                 :cell-factory (fn [action]
                                                 {:text (:label action)
                                                  :on-mouse-entered {::event/handler on-item-hover
                                                                     :id id
                                                                     :action action}
                                                  :on-mouse-clicked {::event/handler on-item-clicked
                                                                     :action action
                                                                     :segments segments
                                                                     :on-cancel on-cancel}})
                                 :items actions}}]
                        (pos? (count filter-text))
                        (conj {:fx/type :label
                               :mouse-transparent true
                               :stack-pane/alignment (if popup-at-the-bottom :top-right :bottom-right)
                               :style-class "search-label"
                               :text filter-text}))}
               {:fx/type :v-box
                :max-height max-content-height
                :style-class "reveal-popup"
                :on-key-pressed {::event/handler on-result-key-pressed :on-cancel on-cancel}
                :children [{:fx/type :label
                            :style {:-fx-text-fill :red}
                            :text (str output)}]}))

           (cond-> (not popup-at-the-bottom)
                   (conj {:fx/type :polygon
                          :v-box/margin {:left (- arrow-x (* arrow-width 0.5))}
                          :fill (::style/popup-color style/style)
                          :points [0 0
                                   arrow-width 0
                                   (* arrow-width 0.5) arrow-height]})))}]}))