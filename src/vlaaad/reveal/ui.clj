(ns vlaaad.reveal.ui
  (:require [cljfx.api :as fx]
            [vlaaad.reveal.style :as style]
            [vlaaad.reveal.output-panel :as output-panel]
            [vlaaad.reveal.segment :as segment]
            [vlaaad.reveal.stream :as stream]
            [vlaaad.reveal.event :as event]
            [vlaaad.reveal.view :as view]
            [cljfx.lifecycle :as fx.lifecycle])
  (:import [javafx.scene.input KeyEvent KeyCode MouseEvent]
           [javafx.scene Node]
           [javafx.beans.value ChangeListener]
           [javafx.event Event]
           [java.util.concurrent ArrayBlockingQueue]
           [java.util UUID List]
           [clojure.lang MultiFn]))

(defn- on-header-width-changed [{:keys [state id fx/event]}]
  {:state (assoc-in state [:views id :header-width] event)})

(defn- on-header-height-changed [{:keys [state id fx/event]}]
  {:state (assoc-in state [:views id :header-height] event)})

(defn- on-view-key-pressed [{:keys [id ^KeyEvent fx/event]}]
  (when (= KeyCode/ESCAPE (.getCode event))
    {:close-view id}))

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

(defn- request-focus! [^MouseEvent e]
  (.requestFocus ^Node (.getTarget e)))

(defn- view [{:keys [showing output view-order views focused-view] :as state}]
  (let [has-views (pos? (count view-order))]
    {:fx/type :stage
     ;; todo ask if user wants to quit repl (default) or exit jvm
     :on-close-request #(.consume ^Event %)
     :showing showing
     :width 400
     :height 500
     :on-focused-changed {::event/handler on-window-focused-changed}
     :scene {:fx/type :scene
             :stylesheets [(:cljfx.css/url style/style)]
             :root {:fx/type :grid-pane
                    :style-class "reveal-ui"
                    :column-constraints [{:fx/type :column-constraints
                                          :hgrow :always}]
                    :row-constraints (if has-views
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
                            has-views
                            (conj
                              {:fx/type :v-box
                               :grid-pane/row 1
                               :grid-pane/column 0
                               :on-key-pressed {::event/handler on-view-key-pressed :id focused-view}
                               :children [{:fx/type :h-box
                                           :style-class "reveal-view-header"
                                           :children (->> view-order
                                                          (map (fn [id]
                                                                 {:fx/type fx/ext-on-instance-lifecycle
                                                                  :fx/key id
                                                                  :on-created #(.put (.getProperties ^Node %) ::id id)
                                                                  :desc {:fx/type :group
                                                                         :children [{:fx/type segment/view
                                                                                     :width (get-in views [id :header-width])
                                                                                     :on-width-changed {::event/handler on-header-width-changed :id id}
                                                                                     :height (get-in views [id :header-height])
                                                                                     :on-height-changed {::event/handler on-header-height-changed :id id}
                                                                                     :segments (get-in views [id :action :segments])}]}}))
                                                          (interpose {:fx/type :label
                                                                      :style-class "reveal-view-header-separator"
                                                                      :min-width :use-pref-size
                                                                      :text "→"}))}
                                          {:fx/type ext-focused-by-default
                                           :fx/key focused-view
                                           :v-box/vgrow :always
                                           :desc (get state focused-view)}]}))}}}))

(defn oneduce
  ([xf x]
   (oneduce xf identity x))
  ([xf f x]
   (let [rf (xf (completing #(f %2)))]
     (rf (rf nil x)))))

(defn- conj-fn [coll x]
  (cond-> coll (ifn? x) (conj x)))

(defn on-open-view [{:keys [state id desc action value *view-state]}]
  {:state (-> state
              (assoc id desc :focused-view id)
              (update :view-order conj id)
              (assoc-in [:views id] {:action action
                                     :header-width 0
                                     :header-height 0
                                     :value value
                                     :*view-state *view-state}))})

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
  (fn [id _]
    (let [{:keys [*view-state]} (get-in @*state [:views id])]
      (doseq [f (:on-delete @*view-state)]
        (f))
      (swap! *state (fn [state]
                      (let [^List view-order (:view-order state)
                            i (.indexOf view-order id)
                            new-view-order (into (subvec view-order 0 i) (subvec view-order (inc i)))]
                        (-> state
                            (dissoc id)
                            (assoc :view-order new-view-order)
                            (assoc :focused-view (get new-view-order (max 0 (dec i))))
                            (update :views dissoc id)))))
      (swap! *view-state assoc :state :stopped))))

(defn- add-lines [{:keys [state fx/event]}]
  {:state (update state :output output-panel/add-lines event)})

(defn make []
  (let [out-queue (ArrayBlockingQueue. 1024)
        xform (comp stream/stream-xf
                    (partition-all 128))
        output! #(.put out-queue (if (nil? %) ::stream-nil %))
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
        output-thread (doto (Thread.
                              ^Runnable
                              (fn []
                                (while (not (Thread/interrupted))
                                  (try
                                    (let [x (.take out-queue)
                                          x (if (= ::stream-nil x) nil x)]
                                      (oneduce xform add-lines! x))
                                    (catch InterruptedException _))))
                              "reveal-stream-thread")
                        (.setDaemon true))
        renderer (fx/create-renderer
                   :opts {:fx.opt/map-event-handler event-handler
                          :fx.opt/type->lifecycle #(or (fx/keyword->lifecycle %)
                                                       (fx/fn->lifecycle %)
                                                       (when (instance? MultiFn %)
                                                         fx.lifecycle/dynamic-fn->dynamic))}
                   :middleware (fx/wrap-map-desc #(view %)))]
    (.start output-thread)
    (fx/mount-renderer *state renderer)
    (fn
      ([]
       (fx/unmount-renderer *state renderer)
       (.interrupt output-thread)
       (.clear out-queue))
      ([x]
       (output! x)
       x))))
