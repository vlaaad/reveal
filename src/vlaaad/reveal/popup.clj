(ns vlaaad.reveal.popup
  (:require [vlaaad.reveal.event :as event]
            [cljfx.fx.popup :as fx.popup]
            [vlaaad.reveal.search :as search]
            [cljfx.composite :as fx.composite]
            [cljfx.lifecycle :as fx.lifecycle]
            [cljfx.mutator :as fx.mutator]
            [vlaaad.reveal.style :as style]
            [cljfx.api :as fx]
            [clojure.string :as str]
            [cljfx.prop :as fx.prop])
  (:import [javafx.geometry Bounds Rectangle2D]
           [javafx.stage Screen Popup]
           [com.sun.javafx.event RedirectedEvent]
           [javafx.event Event]
           [javafx.scene.input KeyCode KeyEvent]
           [java.util Collection List]
           [javafx.beans.value ChangeListener]
           [javafx.scene Node]))

(defn- consume-popup-event [^Event e]
  (if (instance? RedirectedEvent e)
    (.consume (.getOriginalEvent ^RedirectedEvent e))
    (.consume e)))

(def displayed-actions
  (some-fn :selected-actions :actions))

(defn- move-selected-index [this direction]
  (let [actions (displayed-actions this)]
    (update this :selected-index
            (fn [i]
              (let [min 0
                    max (dec (count actions))
                    i (cond
                        i (direction i)
                        (= direction inc) min
                        :else max)]
                (when (<= min i max) i))))))

(defn- select-action [this action]
  (let [^List actions (displayed-actions this)
        index (.indexOf actions action)]
    (assoc this :selected-index (when-not (neg? index) index))))

(defn- set-text [{:keys [actions] :as this} text]
  (-> this
      (assoc :text text
             :selected-actions (cond-> actions
                                       (not= "" text)
                                       (search/select text :label)))
      (dissoc :selected-index)))

(defn- state-fx [state id f & args]
  {:state (apply update-in state [id :popup] f args)})

(defn- on-action-key-pressed [{:keys [state id ^KeyEvent fx/event action on-cancel segments]}]
  (condp = (.getCode event)
    KeyCode/ESCAPE
    {:dispatch on-cancel}

    KeyCode/ENTER
    [[:dispatch on-cancel]
     [:execute (assoc action :segments segments)]]

    KeyCode/UP
    (state-fx state id move-selected-index dec)

    KeyCode/DOWN
    (state-fx state id move-selected-index inc)

    nil))

(defn- on-action-pressed [{:keys [state id action]}]
  (state-fx state id select-action action))

(defn- on-action-clicked [{:keys [action segments on-cancel]}]
  [[:dispatch on-cancel]
   [:execute (assoc action :segments segments)]])

(defn- on-text-key-pressed [{:keys [state id ^KeyEvent fx/event on-cancel segments val ann]}]
  (let [this (get-in state [id :popup])]
    (condp = (.getCode event)
      KeyCode/ESCAPE
      (if (empty? (:text this))
        {:dispatch on-cancel}
        (state-fx state id set-text ""))

      KeyCode/ENTER
      (when-not (str/blank? (:text this))
        {:execute {:invoke (fn []
                             (let [form (read-string (:text this))
                                   form `(fn [~'*v ~'*a] ~(cond
                                                            ('#{*a *v} form) form
                                                            (symbol? form) (list form '*v)
                                                            :else form))]
                               ((eval form) val ann)))
                   :segments segments}
         :dispatch on-cancel})

      KeyCode/UP
      (state-fx state id move-selected-index dec)

      KeyCode/DOWN
      (state-fx state id move-selected-index inc)

      nil)))

(defn- on-text-changed [{:keys [state id fx/event]}]
  (state-fx state id set-text event))

(defn- on-text-focused [{:keys [state id fx/event]}]
  (when event (state-fx state id dissoc :selected-index)))

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

(defn focus-when-on-scene! [^Node node]
  (if (some? (.getScene node))
    (.requestFocus node)
    (.addListener (.sceneProperty node)
                  (reify ChangeListener
                    (changed [this _ _ new-scene]
                      (when (some? new-scene)
                        (.removeListener (.sceneProperty node) this)
                        (.requestFocus node)))))))

(def ext-with-focused-ref
  (fx/make-ext-with-props
    {:focused-ref (fx.prop/make (fx.mutator/setter
                                  (fn [_ ^Node node]
                                    (some-> node focus-when-on-scene!)))
                                (fx.lifecycle/get-ref identity))}))

(defn view [{:keys [val
                    ann
                    ^Bounds bounds
                    id
                    on-cancel
                    text
                    selected-index
                    segments]
             :or {text ""}
             :as this}]
  (let [actions (displayed-actions this)
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
                              arrow-height)
        action-view {:fx/type :scroll-pane
                     :style-class "reveal-popup-scroll-pane"
                     :fit-to-width true
                     :content {:fx/type :v-box
                               :fill-width true
                               :children (map
                                           (fn [action]
                                             {:fx/type fx/ext-get-ref
                                              :fx/key (:id action)
                                              :ref [::action (:id action)]})
                                           actions)}}]
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
             {:fx/type fx/ext-let-refs
              :refs (into {::text-field {:fx/type :text-field
                                         :style-class "reveal-popup-text-field"
                                         :text text
                                         :on-focused-changed {::event/handler on-text-focused
                                                              :id id}
                                         :on-key-pressed {::event/handler on-text-key-pressed
                                                          :id id
                                                          :val val
                                                          :ann ann
                                                          :on-cancel on-cancel
                                                          :segments segments}
                                         :on-text-changed {::event/handler on-text-changed
                                                           :fx/sync true
                                                           :id id}}}
                          (map-indexed
                            (fn [i action]
                              [[::action (:id action)]
                               {:fx/type :label
                                :style-class (cond-> ["reveal-popup-item"]
                                                     (= i selected-index)
                                                     (conj "reveal-popup-item-selected"))
                                :min-width :use-pref-size
                                :text (:label action)
                                :on-key-pressed {::event/handler on-action-key-pressed
                                                 :id id
                                                 :action action
                                                 :segments segments
                                                 :on-cancel on-cancel}
                                :on-mouse-pressed {::event/handler on-action-pressed
                                                   :id id
                                                   :action action}
                                :on-mouse-clicked {::event/handler on-action-clicked
                                                   :action action
                                                   :segments segments
                                                   :on-cancel on-cancel}}]))
                          actions)
              :desc {:fx/type ext-with-focused-ref
                     :props {:focused-ref (if selected-index
                                            [::action (:id (actions selected-index))]
                                            ::text-field)}
                     :desc {:fx/type :v-box
                            :style-class "reveal-popup"
                            :max-height max-content-height
                            :children (-> []
                                          (cond-> popup-at-the-bottom
                                                  (conj {:fx/type fx/ext-get-ref
                                                         :ref ::text-field
                                                         :fx/key ::text-field}))
                                          (cond-> (pos? (count actions)) (conj action-view))
                                          (cond-> (not popup-at-the-bottom)
                                                  (conj {:fx/type fx/ext-get-ref
                                                         :ref ::text-field
                                                         :fx/key ::text-field})))}}})
           (cond-> (not popup-at-the-bottom)
                   (conj {:fx/type :polygon
                          :v-box/margin {:left (- arrow-x (* arrow-width 0.5))}
                          :fill (::style/popup-color style/style)
                          :points [0 0
                                   arrow-width 0
                                   (* arrow-width 0.5) arrow-height]})))}]}))