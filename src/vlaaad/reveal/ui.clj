(ns vlaaad.reveal.ui
  (:require [cljfx.api :as fx]
            [vlaaad.reveal.style :as style]
            [vlaaad.reveal.event :as event]
            [vlaaad.reveal.focus-tree :as focus-tree]
            [vlaaad.reveal.stream :as stream]
            [vlaaad.reveal.view :as view])
  (:import [javafx.scene.input KeyEvent KeyCode]
           [javafx.scene Node Parent]
           [javafx.beans.value ChangeListener]
           [javafx.event Event]
           [java.util.concurrent ArrayBlockingQueue]
           [java.util UUID]))

(defn- remove-index [xs i]
  (into (subvec xs 0 i) (subvec xs (inc i))))

(defmethod event/handle ::on-view-event [{:keys [^Event fx/event index]}]
  (if (and (instance? KeyEvent event)
           (= KeyEvent/KEY_PRESSED (.getEventType event)))
    (let [^KeyEvent event event
          code (.getCode event)]
      (cond
        (and (= KeyCode/LEFT code) (.isShortcutDown event))
        (do
          (.consume event)
          #(update-in % [:result-trees index] focus-tree/focus-prev))

        (and (= KeyCode/RIGHT code) (.isShortcutDown event))
        (do
          (.consume event)
          #(update-in % [:result-trees index] focus-tree/focus-next))

        (and (= KeyCode/ESCAPE code)
             (not (::consumes-escape (.getProperties ^Node (.getTarget event)))))
        (do
          (.consume event)
          (fn [state]
            (let [result-trees (:result-trees state)
                  tree (result-trees index)
                  id (::focus-tree/id tree)
                  new-tree (focus-tree/close tree)]
              (-> (if new-tree
                    (assoc-in state [:result-trees index] new-tree)
                    (update state :result-trees remove-index index))
                  (dissoc id)
                  (update :views dissoc id)))))

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

(defmethod event/handle ::change-result-focus [{:keys [fn index]}]
  #(update-in % [:result-trees index] fn))

(defn- result-tree-view [{:keys [views result-tree index]}]
  (let [{::focus-tree/keys [id depth]} result-tree
        {:keys [form]} (get views id)]
    {:fx/type :v-box
     :event-filter {::event/type ::on-view-event :index index}
     :children [{:fx/type :h-box
                 :style-class "reveal-view-header"
                 :alignment :center
                 :children (interpose
                             {:fx/type :region
                              :style-class "reveal-view-header-separator"}
                             [{:fx/type :button
                               :style-class "reveal-view-header-button"
                               :disable (not (focus-tree/has-prev? result-tree))
                               :text "<"
                               :on-action {::event/type ::change-result-focus
                                           :index index
                                           :fn focus-tree/focus-prev}}
                              {:fx/type :button
                               :style-class "reveal-view-header-button"
                               :disable (not (focus-tree/has-next? result-tree))
                               :text ">"
                               :on-action {::event/type ::change-result-focus
                                           :index index
                                           :fn focus-tree/focus-next}}
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
                                                      (apply str (repeat (get depth id) "·"))
                                                      {:fill :util})
                                                    (stream/stream form)))}}
                              #_{:fx/type :button
                                 :style-class "reveal-view-header-button"
                                 :text "↕"}])}
                {:fx/type ext-focused-by-default
                 :fx/key id
                 :v-box/vgrow :always
                 :desc {:fx/type fx/ext-get-ref
                        :ref id}}]}))

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

(defn- view [{:keys [title queue showing views result-trees confirm-exit-showing dispose]}]
  {:fx/type fx/ext-let-refs
   :refs (into {}
               (for [i (range (count result-trees))
                     id (::focus-tree/order (result-trees i))]
                 [id {:fx/type fx/ext-set-env
                      :env {::id id ::index i}
                      :desc {:fx/type view/ext-try
                             :desc {:fx/type view/derefable
                                    :derefable (get-in views [id :future])}}}]))
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
                                         :row-constraints (let [n (inc (count result-trees))]
                                                            (repeat n {:fx/type :row-constraints
                                                                       :percent-height (/ 100 n)}))
                                         :children
                                         (into [{:fx/type view/queue
                                                 :grid-pane/row 0
                                                 :grid-pane/column 0
                                                 :queue queue
                                                 :id :output}]
                                               (map-indexed
                                                 (fn [i result-tree]
                                                   {:fx/type result-tree-view
                                                    :grid-pane/row (inc i)
                                                    :grid-pane/column 0
                                                    :views views
                                                    :index i
                                                    :result-tree result-tree}))
                                               result-trees)}}}}
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

(defmethod event/handle ::execute-action [{:keys [action view-id view-index new-result-tree]}]
  (let [id (UUID/randomUUID)
        f (future ((:invoke action)))]
    (fn [state]
      (let [result-trees (:result-trees state)
            index (if new-result-tree (count result-trees) (or view-index 0))]
        (-> state
            (assoc :result-trees (update result-trees index focus-tree/add view-id id))
            (assoc-in [:views id] {:form (:form action) :future f}))))))

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
                       :result-trees []
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