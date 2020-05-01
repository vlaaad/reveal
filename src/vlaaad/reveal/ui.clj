(ns vlaaad.reveal.ui
  (:require [cljfx.api :as fx]
            [vlaaad.reveal.style :as style]
            [vlaaad.reveal.segment :as segment]
            [vlaaad.reveal.event :as event]
            [vlaaad.reveal.view :as view]
            [cljfx.lifecycle :as fx.lifecycle]
            [cljfx.composite :as fx.composite]
            [cljfx.fx.scroll-pane :as fx.scroll-pane]
            [cljfx.mutator :as fx.mutator]
            [cljfx.prop :as fx.prop])
  (:import [javafx.scene.input KeyEvent KeyCode]
           [javafx.scene Node Parent]
           [javafx.beans.value ChangeListener]
           [javafx.event Event]
           [java.util.concurrent ArrayBlockingQueue]
           [java.util UUID]
           [javafx.scene.control ScrollPane]))

(defmethod event/handle ::on-header-width-changed [*state {:keys [id fx/event]}]
  (swap! *state assoc-in [:views id :header-width] event))

(defmethod event/handle ::on-header-height-changed [*state {:keys [id fx/event]}]
  (swap! *state assoc-in [:views id :header-height] event))

(defmethod event/handle ::on-view-key-pressed [*state {:keys [index ^KeyEvent fx/event]}]
  (let [code (.getCode event)]
    (cond
      (and (= KeyCode/LEFT code) (.isShortcutDown event))
      (swap! *state update :focused-view-index #(max 0 (dec %)))

      (and (= KeyCode/RIGHT code) (.isShortcutDown event))
      (swap! *state (fn [state]
                      (update state :focused-view-index #(min (inc %) (dec (count (:view-order state)))))))

      (= KeyCode/ESCAPE code)
      (swap! *state (fn [state]
                      (let [id (get-in state [:view-order index])
                            view-order (:view-order state)
                            new-view-order (into (subvec view-order 0 index)
                                                 (subvec view-order (inc index)))]
                        (-> state
                            (dissoc id)
                            (assoc :view-order new-view-order)
                            (as-> $ (if (empty? new-view-order)
                                      (dissoc $ :focused-view-index)
                                      (assoc $ :focused-view-index
                                               (min index (dec (count new-view-order))))))
                            (update :views dissoc id))))))))

(defn focus-when-on-scene! [^Node node]
  (if (some? (.getScene node))
    (.requestFocus node)
    (.addListener (.sceneProperty node)
                  (reify ChangeListener
                    (changed [this _ _ new-scene]
                      (when (some? new-scene)
                        (.removeListener (.sceneProperty node) this)
                        (fx/run-later
                          (.requestFocus node))))))))

(defn ext-focused-by-default [{:keys [desc]}]
  {:fx/type fx/ext-on-instance-lifecycle
   :on-created focus-when-on-scene!
   :desc desc})

(defmethod event/handle ::on-window-focused-changed [*state {:keys [fx/event]}]
  (swap! *state assoc :window-focused event))

(defmethod event/handle ::focus-on-tab [*state {:keys [index]}]
  (swap! *state assoc :focused-view-index index))

(def ^:private scroll-pane
  (fx.composite/lifecycle
    {:ctor #(proxy [ScrollPane] []
              (requestFocus []))
     :args []
     :prop-order {:focused-view 1}
     :props (assoc fx.scroll-pane/props
              :focused-view
              (fx.prop/make
                (fx.mutator/setter
                  (fn [^ScrollPane scroll-pane id]
                    (.layout scroll-pane)
                    (when-let [view (->> ^Parent (.getContent scroll-pane)
                                         (.getChildrenUnmodifiable)
                                         (some #(when (= id (::id (.getProperties ^Node %))) %)))]
                      (let [content (.getBoundsInLocal (.getContent scroll-pane))
                            region (.getBoundsInParent view)
                            viewport (.getViewportBounds scroll-pane)
                            viewport-x (.getMinX viewport)
                            viewport-width (.getWidth viewport)
                            region-x (.getMinX region)
                            region-width (.getWidth region)
                            canvas-start (- viewport-x)
                            region-end (+ region-x region-width)
                            canvas-end (+ canvas-start viewport-width)
                            start (if (> region-end canvas-end)
                                    (- region-x (- region-end canvas-end))
                                    region-x)
                            start (max start canvas-start)]
                        (.setHvalue scroll-pane (/ (- region-x start viewport-x)
                                                   (- (.getWidth content) viewport-width)))))))
                fx.lifecycle/scalar))}))

(defn- view [{:keys [queue showing view-order views focused-view-index]}]
  {:fx/type fx/ext-let-refs
   :refs (into {} (map (juxt key #(-> % val :desc)) views))
   :desc
   {:fx/type :stage
    :title "Reveal"
    ;; todo ask if user wants to quit repl (default) or exit jvm
    :on-close-request #(.consume ^Event %)
    :showing showing
    :width 400
    :height 500
    :icons ["logo-16.png" "logo-32.png" "logo-64.png" "logo-256.png" "logo-512.png"]
    :on-focused-changed {::event/type ::on-window-focused-changed}
    :scene {:fx/type :scene
            :stylesheets [(:cljfx.css/url style/style)]
            :root {:fx/type :grid-pane
                   :style-class "reveal-ui"
                   :column-constraints [{:fx/type :column-constraints
                                         :hgrow :always}]
                   :row-constraints (if focused-view-index
                                      [{:fx/type :row-constraints
                                        :percent-height 50}
                                       {:fx/type :row-constraints
                                        :percent-height 50}]
                                      [{:fx/type :row-constraints
                                        :percent-height 100}])
                   :children
                   (cond-> [{:fx/type view/queue
                             :grid-pane/row 0
                             :grid-pane/column 0
                             :queue queue
                             :id :output}]
                           focused-view-index
                           (conj
                             (let [focused-view-id (view-order focused-view-index)]
                               {:fx/type :v-box
                                :grid-pane/row 1
                                :grid-pane/column 0
                                :on-key-pressed {::event/type ::on-view-key-pressed :index focused-view-index}
                                :children [{:fx/type scroll-pane
                                            :style-class "reveal-view-scroll-pane"
                                            :fit-to-width true
                                            :vbar-policy :never
                                            :hbar-policy :never
                                            :focused-view focused-view-id
                                            :content
                                            {:fx/type :h-box
                                             :style-class "reveal-view-header"
                                             :min-width :use-pref-size
                                             :children
                                             (->> view-order
                                                  (map-indexed
                                                    (fn [index id]
                                                      {:fx/type fx/ext-on-instance-lifecycle
                                                       :fx/key id
                                                       :on-created #(.put (.getProperties ^Node %) ::id id)
                                                       :desc {:fx/type :anchor-pane
                                                              :style-class ["reveal-view-header-tab" (str "reveal-view-header-tab-" (if (= id focused-view-id) "focused" "blurred"))]
                                                              :min-width :use-pref-size
                                                              :min-height :use-pref-size
                                                              :on-mouse-clicked {::event/type ::focus-on-tab :index index}
                                                              :children [{:fx/type segment/view
                                                                          :anchor-pane/left 5
                                                                          :anchor-pane/right 5
                                                                          :anchor-pane/top 5
                                                                          :anchor-pane/bottom 5
                                                                          :width (get-in views [id :header-width])
                                                                          :on-width-changed {::event/type ::on-header-width-changed :id id}
                                                                          :height (get-in views [id :header-height])
                                                                          :on-height-changed {::event/type ::on-header-height-changed :id id}
                                                                          :segments (get-in views [id :action :segments])}]}}))
                                                  (interpose
                                                    {:fx/type :region
                                                     :style-class "reveal-view-header-separator"}))}}
                                           {:fx/type ext-focused-by-default
                                            :fx/key focused-view-id
                                            :v-box/vgrow :always
                                            :desc {:fx/type fx/ext-get-ref
                                                   :ref focused-view-id}}]})))}}}})

(defn oneduce
  ([xf x]
   (oneduce xf identity x))
  ([xf f x]
   (let [rf (xf (completing #(f %2)))]
     (rf (rf nil x)))))

(defmethod event/handle ::on-open-view-2 [*state {:keys [action]}]
  (let [id (UUID/randomUUID)]
    (future ;; todo future-view!
      (let [desc (view/make (try
                              ((:invoke action))
                              (catch Throwable e
                                ;; todo show errors differently
                                e)))]
        (swap! *state (fn [state]
                        (let [view-order (:view-order state)]
                          (-> state
                              (assoc :focused-view-index (count view-order)
                                     :view-order (conj view-order id))
                              (assoc-in [:views id] {:action action :desc desc})))))))))

(defn- stop-queue [_ ^ArrayBlockingQueue queue]
  (.clear queue)
  false)

(defn- put-on-queue [running ^ArrayBlockingQueue queue x]
  (when running
    (.put queue ({nil ::view/nil} x x)))
  running)

(defn make []
  (let [*running! (agent true)
        value-queue (ArrayBlockingQueue. 1024)
        *state (atom {:queue value-queue :views {} :view-order [] :showing true})
        event-handler (event/->MapEventHandler *state)
        renderer (fx/create-renderer
                   :opts {:fx.opt/map-event-handler event-handler}
                   :middleware (fx/wrap-map-desc #'view))]
    (fx/mount-renderer *state renderer)
    (fn
      ([]
       (fx/unmount-renderer *state renderer)
       (send-via event/daemon-executor *running! stop-queue value-queue))
      ([x]
       (send-via event/daemon-executor *running! put-on-queue value-queue x)
       x))))
