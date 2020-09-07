(ns vlaaad.reveal.popup
  (:require [vlaaad.reveal.event :as event]
            [cljfx.fx.popup :as fx.popup]
            [vlaaad.reveal.search :as search]
            [cljfx.composite :as fx.composite]
            [cljfx.lifecycle :as fx.lifecycle]
            [cljfx.mutator :as fx.mutator]
            [vlaaad.reveal.style :as style]
            [vlaaad.reveal.fx :as rfx]
            [cljfx.api :as fx]
            [clojure.string :as str]
            [cljfx.prop :as fx.prop]
            [clojure.walk :as walk]
            [vlaaad.reveal.action :as action]
            [cljfx.fx.node :as fx.node]
            [vlaaad.reveal.stream :as stream])
  (:import [javafx.geometry Bounds Rectangle2D]
           [javafx.stage Screen Popup Window]
           [com.sun.javafx.event RedirectedEvent]
           [javafx.event Event]
           [javafx.scene.input KeyCode KeyEvent ContextMenuEvent]
           [java.util Collection List]
           [javafx.beans.value ChangeListener]
           [javafx.scene Node]))

(set! *warn-on-reflection* true)

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

(defmethod event/handle ::on-action-key-pressed [{:keys [id ^KeyEvent fx/event action on-cancel]}]
  (condp = (.getCode event)
    KeyCode/ESCAPE (event/handle on-cancel)
    KeyCode/ENTER (comp
                    (event/handle on-cancel)
                    (event/handle {::event/type :vlaaad.reveal.ui/execute-action
                                   :action action}))
    KeyCode/UP #(update % id move-selected-index dec)
    KeyCode/DOWN #(update % id move-selected-index inc)
    identity))

(defmethod event/handle ::on-action-pressed [{:keys [id action]}]
  #(update % id select-action action))

(defmethod event/handle ::on-action-clicked [{:keys [action on-cancel]}]
  (comp
    (event/handle {::event/type :vlaaad.reveal.ui/execute-action
                   :action action})
    (event/handle on-cancel)))

(defmethod event/handle ::on-text-key-pressed [{:keys [text id ^KeyEvent fx/event on-cancel annotated-value]}]
  (let [value (:value annotated-value)]
    (condp = (.getCode event)
      KeyCode/ESCAPE
      (if (empty? text)
        (event/handle on-cancel)
        #(update % id set-text ""))

      KeyCode/ENTER
      (if-not (str/blank? text)
        (try
          (let [form (read-string text)
                fn-form `(fn [~'*v]
                           ~(cond
                              (= '*v form) form
                              (ident? form) (list form '*v)
                              :else form))]
            (comp
              (event/handle {::event/type :vlaaad.reveal.ui/execute-action
                             :action {:form (cond
                                              (= '*v form) (list 'identity value)
                                              (ident? form) (list form value)
                                              :else (walk/prewalk-replace
                                                      {'*v value}
                                                      form))
                                      :invoke (fn []
                                                ((eval fn-form) value))}})
              (event/handle on-cancel)))
          (catch Exception _ identity))
        identity)

      KeyCode/UP
      #(update % id move-selected-index dec)

      KeyCode/DOWN
      #(update % id move-selected-index inc)

      identity)))

(defmethod event/handle ::on-text-changed [{:keys [id fx/event]}]
  #(update % id set-text event))

(defmethod event/handle ::on-text-focused [{:keys [id fx/event]}]
  (if event
    #(update % id dissoc :selected-index)
    identity))

(def ^:private lifecycle
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

(defn- view-impl [{:keys [annotated-value
                          ^Bounds bounds
                          window
                          id
                          on-cancel
                          text
                          selected-index]
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
     :window window
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
                          :fill style/popup-color
                          :points [0 arrow-height
                                   arrow-width arrow-height
                                   (* arrow-width 0.5) 0]}))
           (conj
             {:fx/type fx/ext-let-refs
              :refs (into {::text-field {:fx/type :text-field
                                         :style-class "reveal-text-field"
                                         :text text
                                         :on-focused-changed {::event/type ::on-text-focused
                                                              :id id}
                                         :on-key-pressed {::event/type ::on-text-key-pressed
                                                          :text text
                                                          :id id
                                                          :annotated-value annotated-value
                                                          :on-cancel on-cancel}
                                         :on-text-changed {::event/type ::on-text-changed
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
                                :on-key-pressed {::event/type ::on-action-key-pressed
                                                 :id id
                                                 :action action
                                                 :on-cancel on-cancel}
                                :on-mouse-pressed {::event/type ::on-action-pressed
                                                   :id id
                                                   :action action}
                                :on-mouse-clicked {::event/type ::on-action-clicked
                                                   :action action
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
                          :fill style/popup-color
                          :points [0 0
                                   arrow-width 0
                                   (* arrow-width 0.5) arrow-height]})))}]}))

(defmethod event/handle ::init-popup [{:keys [id actions]}]
  #(assoc % id {:actions actions :id id}))

(defmethod event/handle ::dispose-popup [{:keys [id]}]
  #(dissoc % id))

(defn- init-popup! [id annotated-value handler]
  (handler {::event/type ::init-popup :id id :actions (action/collect annotated-value)})
  #(handler {::event/type ::dispose-popup :id id}))

(defn view [{:keys [annotated-value bounds window id on-cancel]
             :or {id ::rfx/undefined}}]
  {:fx/type rfx/ext-with-process
   :id id
   :args annotated-value
   :start init-popup!
   :desc {:fx/type view-impl
          :annotated-value annotated-value
          :on-cancel on-cancel
          :bounds bounds
          :window window}})

(defmethod event/handle ::init-ext [{:keys [id]}]
  #(assoc % id {:id id}))

(defmethod event/handle ::dispose-ext [{:keys [id]}]
  #(dissoc % id))

(defmethod event/handle ::on-popup-node-event [{:keys [id select ^Event fx/event]}]
  (if (or (instance? ContextMenuEvent event)
          (and (instance? KeyEvent event)
               (or (= KeyCode/SPACE (.getCode ^KeyEvent event))
                   (= KeyCode/ENTER (.getCode ^KeyEvent event)))
               (= KeyEvent/KEY_PRESSED (.getEventType event))))
    (let [node ^Node (.getSource event)]
      (if select
        (if-let [{:keys [bounds annotation value]} (select event)]
          (do (.consume event)
              #(update % id assoc
                       :value value
                       :annotation annotation
                       :bounds bounds
                       :window (.getWindow (.getScene node))))
          identity)
        (do
          (.consume event)
          #(update % id assoc
                   :bounds (.localToScreen node (.getBoundsInLocal node))
                   :window (.getWindow (.getScene node))))))
    identity))

(defmethod event/handle ::close [{:keys [id]}]
  #(update % id dissoc :bounds :window :annotated-value))

(defn- init-ext! [id _ handler]
  (handler {::event/type ::init-ext :id id})
  #(handler {::event/type ::dispose-ext :id id}))

(def ^:private ext-with-popup-on-node-props
  (fx/make-ext-with-props
    (assoc fx.node/props
      :popup (fx.prop/make
               (fx.mutator/adder-remover
                 (fn [^Node node ^Popup popup]
                   (.show popup (.getWindow (.getScene node))))
                 (fn [_ ^Popup popup]
                   (.hide popup)))
               fx.lifecycle/dynamic))))

(defn- ext-impl [{:keys [desc annotation value id bounds window select]}]
  {:fx/type ext-with-popup-on-node-props
   :props (cond-> {:event-filter {::event/type ::on-popup-node-event
                                  :id id
                                  :select select}
                   :focus-traversable true}
            bounds
            (assoc :popup {:fx/type view
                           :annotated-value (stream/->AnnotatedValue
                                              value
                                              (assoc annotation ::stream/hidden true))
                           :bounds bounds
                           :window window
                           :id [id :popup]
                           :on-cancel {::event/type ::close :id id}}))
   :desc desc})

(defn ext [props]
  {:fx/type rfx/ext-with-process
   :start init-ext!
   :desc (assoc props :fx/type ext-impl)})