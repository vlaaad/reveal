(ns vlaaad.reveal.ui
  (:require [cljfx.api :as fx]
            [vlaaad.reveal.style :as style]
            [vlaaad.reveal.output-panel :as output-panel]
            [vlaaad.reveal.segment :as segment]
            [vlaaad.reveal.stream :as stream]
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
           [clojure.lang MultiFn]
           [javafx.scene.control ScrollPane]))

(defn- on-header-width-changed [{:keys [state id fx/event]}]
  {:state (assoc-in state [:views id :header-width] event)})

(defn- on-header-height-changed [{:keys [state id fx/event]}]
  {:state (assoc-in state [:views id :header-height] event)})

(defn- on-view-key-pressed [{:keys [index ^KeyEvent fx/event state]}]
  (let [code (.getCode event)]
    (cond
      (and (= KeyCode/LEFT code) (.isShortcutDown event))
      {:state (update state :focused-view-index #(max 0 (dec %)))}

      (and (= KeyCode/RIGHT code) (.isShortcutDown event))
      {:state (update state :focused-view-index #(min (inc %) (dec (count (:view-order state)))))}

      (= KeyCode/ESCAPE code)
      {:close-view index})))

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

(defn- on-window-focused-changed [{:keys [state fx/event]}]
  {:state (assoc state :window-focused event)})

(defn- focus-on-tab [{:keys [state index]}]
  {:state (assoc state :focused-view-index index)})

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

(defn- view [{:keys [showing output view-order views focused-view-index] :as state}]
  {:fx/type :stage
   :title "Reveal"
   ;; todo ask if user wants to quit repl (default) or exit jvm
   :on-close-request #(.consume ^Event %)
   :showing showing
   :width 400
   :height 500
   :icons ["logo-16.png" "logo-32.png" "logo-64.png" "logo-256.png" "logo-512.png"]
   :on-focused-changed {::event/handler on-window-focused-changed}
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
                  (cond-> [(assoc output :fx/type output-panel/view
                                         :grid-pane/row 0
                                         :grid-pane/column 0
                                         :id :output)]
                          focused-view-index
                          (conj
                            (let [focused-view-id (view-order focused-view-index)]
                              {:fx/type :v-box
                               :grid-pane/row 1
                               :grid-pane/column 0
                               :on-key-pressed {::event/handler on-view-key-pressed :index focused-view-index}
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
                                                          :on-mouse-clicked {::event/handler focus-on-tab :index index}
                                                          :children [{:fx/type segment/view
                                                                      :anchor-pane/left 5
                                                                      :anchor-pane/right 5
                                                                      :anchor-pane/top 5
                                                                      :anchor-pane/bottom 5
                                                                      :width (get-in views [id :header-width])
                                                                      :on-width-changed {::event/handler on-header-width-changed :id id}
                                                                      :height (get-in views [id :header-height])
                                                                      :on-height-changed {::event/handler on-header-height-changed :id id}
                                                                      :segments (get-in views [id :action :segments])}]}}))
                                              (interpose
                                                {:fx/type :region
                                                 :style-class "reveal-view-header-separator"}))}}
                                          {:fx/type ext-focused-by-default
                                           :fx/key focused-view-id
                                           :v-box/vgrow :always
                                           :desc (get state focused-view-id)}]})))}}})

(defn oneduce
  ([xf x]
   (oneduce xf identity x))
  ([xf f x]
   (let [rf (xf (completing #(f %2)))]
     (rf (rf nil x)))))

(defn- conj-fn [coll x]
  (cond-> coll (ifn? x) (conj x)))

(defn on-open-view [{:keys [state id desc action value *view-state]}]
  (let [view-order (:view-order state)]
    {:state (-> state
                (assoc id desc
                       :focused-view-index (count view-order)
                       :view-order (conj view-order id))
                (assoc-in [:views id] {:action action
                                       :value value
                                       :*view-state *view-state}))}))

(defn- execute-action-effect [action dispatch!]
  (future
    (let [viewable (try
                     ((:invoke action))
                     (catch Throwable e
                       ;; todo show errors differently
                       e))
          *view-state (atom {:state :init
                             :on-create #{}
                             :on-delete #{}})
          id (UUID/randomUUID)
          env {:id id
               :on-create (fn [f]
                            (if (= :running (:state @*view-state))
                              (swap! *view-state update :on-delete conj-fn (f dispatch!))
                              (swap! *view-state update :on-create conj f)))}
          desc (view/make-view viewable env)]
      (dispatch! {::event/handler on-open-view
                  :fx/sync true
                  :id id
                  :desc desc
                  :action action
                  :value (view/get-value viewable)
                  :*view-state *view-state})
      (let [on-delete (->> @*view-state
                           :on-create
                           (map #(% dispatch!))
                           (filter ifn?)
                           doall)]
        (swap! *view-state #(-> %
                                (assoc :state :running)
                                (update :on-delete into on-delete)))))))

(defn- make-close-view-effect [*state]
  (fn [index _]
    (let [state @*state
          id (get-in state [:view-order index])
          {:keys [*view-state]} (get-in state [:views id])]
      (doseq [f (:on-delete @*view-state)]
        (f))
      (swap! *state (fn [state]
                      (let [view-order (:view-order state)
                            new-view-order (into (subvec view-order 0 index)
                                                 (subvec view-order (inc index)))]
                        (-> state
                            (dissoc id)
                            (assoc :view-order new-view-order)
                            (as-> $ (if (empty? new-view-order)
                                      (dissoc $ :focused-view-index)
                                      (assoc $ :focused-view-index
                                               (min index (dec (count new-view-order))))))
                            (update :views dissoc id)))))
      (swap! *view-state assoc :state :stopped))))

(defn- add-lines [{:keys [state fx/event]}]
  {:state (update state :output output-panel/add-lines event)})

(defn make []
  (let [value-queue (ArrayBlockingQueue. 1024)
        *running (volatile! true)
        xform (comp stream/stream-xf
                    (partition-all 128)
                    (take-while (fn [_] @*running)))
        ;; todo output as view?
        *state (atom {:output (output-panel/make)
                      :views {}
                      :view-order []
                      :showing true})
        event-handler (-> event/handler
                          (fx/wrap-co-effects
                            {:state (fx/make-deref-co-effect *state)})
                          (fx/wrap-effects
                            {:dispatch fx/dispatch-effect
                             :state (fx/make-reset-effect *state)
                             :execute execute-action-effect
                             :close-view (make-close-view-effect *state)})
                          (fx/wrap-async :error-handler #(tap> %2)
                                         :fx/executor event/daemon-executor))
        add-lines! #(event-handler {::event/handler add-lines :fx/event %})
        f (.submit event/daemon-executor ^Runnable
                   (fn []
                     (while @*running
                       (let [x (.take value-queue)]
                         (oneduce xform add-lines! ({::nil nil} x x))))))
        renderer (fx/create-renderer
                   :opts {:fx.opt/map-event-handler event-handler
                          :fx.opt/type->lifecycle #(or (fx/keyword->lifecycle %)
                                                       (fx/fn->lifecycle %)
                                                       (when (instance? MultiFn %)
                                                         fx.lifecycle/dynamic-fn->dynamic))}
                   :middleware (fx/wrap-map-desc #(view %)))]
    (fx/mount-renderer *state renderer)
    (fn
      ([]
       (fx/unmount-renderer *state renderer)
       (vreset! *running false)
       (.cancel f true)
       (.clear value-queue))
      ([x]
       (when @*running
         (.put value-queue ({nil ::nil} x x)))
       x))))
