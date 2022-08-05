(ns vlaaad.reveal.action-popup
  (:require [vlaaad.reveal.event :as event]
            [vlaaad.reveal.popup :as popup]
            [vlaaad.reveal.search :as search]
            [cljfx.lifecycle :as fx.lifecycle]
            [cljfx.mutator :as fx.mutator]
            [vlaaad.reveal.style :as style]
            [vlaaad.reveal.fx :as rfx]
            [cljfx.api :as fx]
            [clojure.string :as str]
            [cljfx.prop :as fx.prop]
            [clojure.walk :as walk]
            [cljfx.fx.scroll-pane :as fx.scroll-pane]
            [cljfx.fx.v-box :as fx.v-box]
            [cljfx.fx.text-field :as fx.text-field]
            [cljfx.fx.label :as fx.label]
            [vlaaad.reveal.action :as action]
            [cljfx.fx.node :as fx.node]
            [vlaaad.reveal.stream :as stream])
  (:import [javafx.geometry Bounds]
           [javafx.stage Popup]
           [javafx.event Event]
           [javafx.scene.input KeyCode KeyEvent ContextMenuEvent MouseEvent]
           [java.util List]
           [javafx.beans.value ChangeListener]
           [javafx.scene Node]))

(set! *warn-on-reflection* true)

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

(defmethod event/handle ::on-action-key-pressed
  [{:keys [id ^KeyEvent fx/event action on-cancel view-id view-index]}]
  (condp = (.getCode event)
    KeyCode/ESCAPE (event/handle on-cancel)
    KeyCode/ENTER (comp
                    (event/handle on-cancel)
                    (event/handle {::event/type :vlaaad.reveal.ui/execute-action
                                   :view-id view-id
                                   :view-index view-index
                                   :target (cond
                                             (.isShiftDown event) :inspector
                                             (.isShortcutDown event) :new-result-panel)
                                   :action action}))
    KeyCode/UP #(update % id move-selected-index dec)
    KeyCode/DOWN #(update % id move-selected-index inc)
    identity))

(defmethod event/handle ::on-action-pressed [{:keys [id action]}]
  #(update % id select-action action))

(defmethod event/handle ::on-action-clicked
  [{:keys [action on-cancel view-id view-index ^MouseEvent fx/event]}]
  (comp
    (event/handle {::event/type :vlaaad.reveal.ui/execute-action
                   :view-id view-id
                   :view-index view-index
                   :target (cond
                             (.isShiftDown event) :inspector
                             (.isShortcutDown event) :new-result-panel)
                   :action action})
    (event/handle on-cancel)))

(defmethod event/handle ::on-text-key-pressed
  [{:keys [text id ^KeyEvent fx/event on-cancel annotated-value view-id view-index]}]
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
                             :view-id view-id
                             :view-index view-index
                             :target (cond
                                       (.isShiftDown event) :inspector
                                       (.isShortcutDown event) :new-result-panel)
                             :action {:form (cond
                                              (= '*v form) (list 'identity value)
                                              (ident? form) (list form value)
                                              :else (walk/postwalk-replace
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
                          view-id
                          view-index
                          text
                          selected-index]
                   :or {text ""}
                   :as this}]
  (let [actions (displayed-actions this)
        bottom (popup/more-screen-space-below? bounds)
        action-view {:fx/type fx.scroll-pane/lifecycle
                     :style-class "reveal-popup-scroll-pane"
                     :fit-to-width true
                     :content {:fx/type fx.v-box/lifecycle
                               :fill-width true
                               :children (map
                                           (fn [action]
                                             {:fx/type fx/ext-get-ref
                                              :fx/key (:id action)
                                              :ref [::action (:id action)]})
                                           actions)}}]
    {:fx/type popup/view
     :bounds bounds
     :window window
     :position (if bottom :bottom :top)
     :on-cancel on-cancel
     :desc {:fx/type fx/ext-let-refs
            :refs (into {::text-field {:fx/type fx.text-field/lifecycle
                                       :text-formatter rfx/code-text-formatter
                                       :text text
                                       :on-focused-changed {::event/type ::on-text-focused
                                                            :id id}
                                       :on-key-pressed {::event/type ::on-text-key-pressed
                                                        :view-id view-id
                                                        :view-index view-index
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
                             {:fx/type fx.label/lifecycle
                              :style-class (cond-> ["reveal-popup-item"]
                                             (= i selected-index)
                                             (conj "reveal-popup-item-selected"))
                              :min-width :use-pref-size
                              :text (:label action)
                              :on-key-pressed {::event/type ::on-action-key-pressed
                                               :view-id view-id
                                               :view-index view-index
                                               :id id
                                               :action action
                                               :on-cancel on-cancel}
                              :on-mouse-pressed {::event/type ::on-action-pressed
                                                 :view-id view-id
                                                 :view-index view-index
                                                 :id id
                                                 :action action}
                              :on-mouse-clicked {::event/type ::on-action-clicked
                                                 :view-id view-id
                                                 :view-index view-index
                                                 :action action
                                                 :on-cancel on-cancel}}]))
                        actions)
            :desc {:fx/type ext-with-focused-ref
                   :props {:focused-ref (if selected-index
                                          [::action (:id (actions selected-index))]
                                          ::text-field)}
                   :desc {:fx/type fx.v-box/lifecycle
                          :spacing style/default-padding
                          :padding style/default-padding
                          :children (-> []
                                        (cond-> bottom
                                          (conj {:fx/type fx/ext-get-ref
                                                 :ref ::text-field
                                                 :fx/key ::text-field}))
                                        (cond-> (pos? (count actions)) (conj action-view))
                                        (cond-> (not bottom)
                                          (conj {:fx/type fx/ext-get-ref
                                                 :ref ::text-field
                                                 :fx/key ::text-field})))}}}}))

(defmethod event/handle ::init-popup [{:keys [id actions]}]
  #(assoc % id {:actions actions :id id}))

(defmethod event/handle ::dispose-popup [{:keys [id]}]
  #(dissoc % id))

(defn- init-popup! [id annotated-value handler]
  (handler {::event/type ::init-popup :id id :actions (action/collect annotated-value)})
  #(handler {::event/type ::dispose-popup :id id}))

(defn- view-with-env [props]
  {:fx/type fx/ext-get-env
   :env {:vlaaad.reveal.ui/id :view-id :vlaaad.reveal.ui/index :view-index}
   :desc (assoc props :fx/type view-impl)})

(defn view [{:keys [annotated-value bounds window id on-cancel]
             :or {id ::rfx/undefined}}]
  {:fx/type rfx/ext-with-process
   :id id
   :args annotated-value
   :start init-popup!
   :desc {:fx/type view-with-env
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
                   :pseudo-classes #{:has-popup}
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