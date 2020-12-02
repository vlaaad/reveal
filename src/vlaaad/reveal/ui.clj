(ns vlaaad.reveal.ui
  (:require [cljfx.api :as fx]
            [vlaaad.reveal.style :as style]
            [vlaaad.reveal.event :as event]
            [vlaaad.reveal.stream :as stream]
            [vlaaad.reveal.view :as view])
  (:import [javafx.scene.input KeyEvent KeyCode]
           [javafx.scene Node Parent]
           [javafx.beans.value ChangeListener]
           [javafx.event Event]
           [java.util.concurrent ArrayBlockingQueue]
           [java.util UUID List]))

(defmethod event/handle ::on-view-event [{:keys [index ^Event fx/event]}]
  (if (and (instance? KeyEvent event)
           (= KeyEvent/KEY_PRESSED (.getEventType event)))
    (let [^KeyEvent event event
          code (.getCode event)]
      (cond
        (and (= KeyCode/LEFT code) (.isShortcutDown event))
        (do
          (.consume event)
          (fn [state]
            (update state :focused-view-index #(max 0 (dec %)))))

        (and (= KeyCode/RIGHT code) (.isShortcutDown event))
        (do
          (.consume event)
          (fn [state]
            (update state :focused-view-index #(min (inc %) (dec (count (:view-order state)))))))

        (and (= KeyCode/ESCAPE code)
             (not (::consumes-escape (.getProperties ^Node (.getTarget event)))))
        (do
          (.consume event)
          (fn [state]
            (let [{:keys [views view-order]} state
                  id (view-order index)
                  depth (:depth (views id))
                  splice-count (count (take-while #(< depth (:depth (views %))) (subvec view-order (inc index))))
                  new-order (into (subvec view-order 0 index)
                                  (subvec view-order (inc index)))
                  new-views (reduce
                              (fn [acc id]
                                (update-in acc [id :depth] dec))
                              (dissoc views id)
                              (subvec view-order (inc index) (+ (inc index) splice-count)))]
              (-> state
                  (dissoc id)
                  (assoc :view-order new-order
                         :views new-views)
                  (as-> $ (if (empty? new-order)
                            (dissoc $ :focused-view-index)
                            (assoc $ :focused-view-index
                                     (min index (dec (count new-order))))))))))

        :else
        identity))
    identity))

(defn- descendant-seq [^Node node]
  (cons node (when (instance? Parent node)
               (mapcat descendant-seq (.getChildrenUnmodifiable ^Parent node)))))

(defn focus-when-on-scene! [^Node node]
  (if (some? (.getScene node))
    (.requestFocus node)
    (.addListener (.sceneProperty node)
                  (reify ChangeListener
                    (changed [this _ _ new-scene]
                      (when (some? new-scene)
                        (.removeListener (.sceneProperty node) this)
                        (fx/run-later
                          (let [^Node node (or (->> node
                                                    descendant-seq
                                                    (some #(when (.isFocusTraversable ^Node %) %)))
                                               node)]
                            (.requestFocus node)))))))))

(defn- switch-focus! [^Node from to]
  (when (.isFocused from)
    (focus-when-on-scene! to)))

(defn ext-focused-by-default [{:keys [desc]}]
  {:fx/type fx/ext-on-instance-lifecycle
   :on-created focus-when-on-scene!
   :on-advanced switch-focus!
   :desc desc})

(defmethod event/handle ::on-window-focused-changed [{:keys [fx/event]}]
  #(assoc % :window-focused event))

(defmethod event/handle ::focus-on-tab [{:keys [index]}]
  #(assoc % :focused-view-index index))

(defn- tabs-view [{:keys [view-order views focused-view-index]}]
  (let [focused-view-id (view-order focused-view-index)
        {:keys [form depth]} (get views focused-view-id)]
    {:fx/type :v-box
     :event-filter {::event/type ::on-view-event :index focused-view-index}
     :children [{:fx/type :h-box
                 :style-class "reveal-view-header"
                 :alignment :center
                 :children (interpose
                             {:fx/type :region
                              :style-class "reveal-view-header-separator"}
                             [{:fx/type :button
                               :style-class "reveal-view-header-button"
                               :disable (zero? focused-view-index)
                               :text "<"
                               :on-action {::event/type ::focus-on-tab :index (dec focused-view-index)}}
                              {:fx/type :button
                               :style-class "reveal-view-header-button"
                               :disable (= focused-view-index (dec (count view-order)))
                               :text ">"
                               :on-action {::event/type ::focus-on-tab :index (inc focused-view-index)}}
                              {:fx/type :button
                               :style-class "reveal-view-header-button"
                               :h-box/hgrow :always
                               :alignment :baseline-left
                               :min-width 0
                               :max-width Double/MAX_VALUE
                               :graphic {:fx/type view/summary
                                         :max-length 128
                                         :value (stream/as-is
                                                  (stream/horizontal
                                                    (stream/raw-string
                                                      (apply str (repeat depth "·"))
                                                      {:fill :util})
                                                    (stream/stream form)))}}
                              #_{:fx/type :button
                                 :style-class "reveal-view-header-button"
                                 :text "↕"}])}
                {:fx/type ext-focused-by-default
                 :fx/key focused-view-id
                 :v-box/vgrow :always
                 :desc {:fx/type fx/ext-get-ref
                        :ref focused-view-id}}]}))

(defmethod event/handle ::confirm-exit [{:keys [^Event fx/event]}]
  (.consume event)
  #(assoc % :confirm-exit-showing true))

(defmethod event/handle ::cancel-quit [_]
  #(dissoc % :confirm-exit-showing))

(defmethod event/handle ::quit [{:keys [dispose]}]
  (dispose)
  #(dissoc % :confirm-exit-showing))

(defn- confirm-exit-dialog [{:keys [dispose]}]
  {:fx/type :stage
   :showing true
   :owner {:fx/type fx/ext-get-ref :ref ::stage}
   :on-close-request {::event/type ::cancel-quit}
   :modality :window-modal
   :title "Quit Reveal?"
   :scene {:fx/type :scene
           :stylesheets [(:cljfx.css/url @style/style)]
           :accelerators {[:escape] {::event/type ::cancel-quit}}
           :root {:fx/type :v-box
                  :style-class "reveal-ui"
                  :spacing style/default-padding
                  :padding style/default-padding
                  :children [{:fx/type :label
                              :text "Are you sure you want to quit Reveal?"}
                             {:fx/type :h-box
                              :spacing 5
                              :alignment :center-right
                              :children [{:fx/type :button
                                          :on-action {::event/type ::cancel-quit}
                                          :text "Cancel"}
                                         {:fx/type ext-focused-by-default
                                          :desc {:fx/type :button
                                                 :on-action {::event/type ::quit
                                                             :dispose dispose}
                                                 :text "Quit"}}]}]}}})

(defn- view [{:keys [title queue showing view-order views focused-view-index confirm-exit-showing dispose]}]
  {:fx/type fx/ext-let-refs
   :refs (into {}
               (map (juxt key (fn [e]
                                {:fx/type fx/ext-set-env
                                 :env {::depth (-> e val :depth)
                                       ::id (key e)}
                                 :desc {:fx/type view/ext-try
                                        :desc {:fx/type view/derefable
                                               :derefable (-> e val :future)}}})))
               views)
   :desc {:fx/type fx/ext-let-refs
          :refs {::stage {:fx/type :stage
                          :title title
                          :on-close-request {::event/type ::confirm-exit}
                          :showing showing
                          :width 400
                          :height 500
                          :icons ["vlaaad/reveal/logo-16.png"
                                  "vlaaad/reveal/logo-32.png"
                                  "vlaaad/reveal/logo-64.png"
                                  "vlaaad/reveal/logo-256.png"
                                  "vlaaad/reveal/logo-512.png"]
                          :on-focused-changed {::event/type ::on-window-focused-changed}
                          :scene {:fx/type :scene
                                  :stylesheets [(:cljfx.css/url @style/style)]
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
                                           (conj {:fx/type tabs-view
                                                  :grid-pane/row 1
                                                  :grid-pane/column 0
                                                  :views views
                                                  :view-order view-order
                                                  :focused-view-index focused-view-index}))}}}}
          :desc {:fx/type fx/ext-let-refs
                 :refs (when confirm-exit-showing
                         {::confirm-exit {:fx/type confirm-exit-dialog
                                          :dispose dispose}})
                 :desc {:fx/type fx/ext-get-ref :ref ::stage}}}})

(defn oneduce
  ([xf x]
   (oneduce xf identity x))
  ([xf f x]
   (let [rf (xf (completing #(f %2)))]
     (rf (rf nil x)))))

(defmethod event/handle ::execute-action [{:keys [action view-id]}]
  (let [id (UUID/randomUUID)
        f (future ((:invoke action)))]
    (fn [state]
      (let [view-order (:view-order state)
            views (:views state)
            parent (views view-id)
            parent-depth (or (:depth parent) -1)
            parent-index (.indexOf ^List view-order view-id)
            insert-index (inc (+ parent-index
                                 (->> (subvec view-order (inc parent-index))
                                      (take-while #(< parent-depth (:depth (views %))))
                                      count)))
            depth (inc parent-depth)]
        (-> state
            (assoc :focused-view-index insert-index
                   :view-order (into (subvec view-order 0 insert-index)
                                     (cons id (subvec view-order insert-index))))
            (assoc-in [:views id] {:form (:form action)
                                   :depth depth
                                   :future f}))))))

(defn- stop-queue [_ ^ArrayBlockingQueue queue]
  (.clear queue)
  false)

(defn- put-on-queue [running ^ArrayBlockingQueue queue x]
  (when running
    (.put queue ({nil ::view/nil} x x)))
  running)

(defn make
  ([] (make {}))
  ([k v & kvs] (make (apply hash-map k v kvs)))
  ([{:keys [title]}]
   (let [*running! (agent true)
         value-queue (ArrayBlockingQueue. 1024)
         *state (atom {:queue value-queue
                       :views {}
                       :view-order []
                       :title (cond-> "Reveal" title (str ": " title))
                       :showing true
                       :dispose (constantly nil)})
         event-handler (event/->MapEventHandler *state)
         renderer (fx/create-renderer
                    :opts {:fx.opt/map-event-handler event-handler}
                    :middleware (fx/wrap-map-desc #'view))
         dispose! #(do
                     (fx/unmount-renderer *state renderer)
                     (send-via event/daemon-executor *running! stop-queue value-queue))]
     (fx/mount-renderer *state renderer)
     (swap! *state assoc :dispose dispose!)
     (fn
       ([]
        (dispose!)
        nil)
       ([x]
        (send-via event/daemon-executor *running! put-on-queue value-queue x)
        x)))))