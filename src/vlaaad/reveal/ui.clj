(ns vlaaad.reveal.ui
  (:require [cljfx.api :as fx]
            [vlaaad.reveal.style :as style]
            [vlaaad.reveal.event :as event]
            [vlaaad.reveal.focus-tree :as focus-tree]
            [vlaaad.reveal.stream :as stream]
            [vlaaad.reveal.popup :as popup]
            [vlaaad.reveal.view :as view]
            [cljfx.prop :as fx.prop]
            [cljfx.mutator :as fx.mutator]
            [cljfx.lifecycle :as fx.lifecycle]
            [vlaaad.reveal.io :as rio]
            [clojure.spec.alpha :as s]
            [cljfx.fx.h-box :as fx.h-box]
            [cljfx.fx.row-constraints :as fx.row-constraints]
            [cljfx.fx.scene :as fx.scene]
            [cljfx.fx.column-constraints :as fx.column-constraints]
            [cljfx.fx.stage :as fx.stage]
            [cljfx.fx.grid-pane :as fx.grid-pane]
            [cljfx.fx.scroll-pane :as fx.scroll-pane]
            [cljfx.fx.v-box :as fx.v-box]
            [cljfx.fx.region :as fx.region]
            [cljfx.fx.button :as fx.button]
            [cljfx.fx.tooltip :as fx.tooltip]
            [cljfx.fx.label :as fx.label])
  (:import [javafx.scene.input KeyEvent KeyCode MouseEvent]
           [javafx.scene Node Parent]
           [javafx.beans.value ChangeListener]
           [javafx.event Event]
           [java.util.concurrent ArrayBlockingQueue TimeUnit]
           [java.util UUID]
           [javafx.geometry Bounds]
           [javafx.scene.control ScrollPane]
           [java.time LocalDate]
           [clojure.lang Namespace]
           [javafx.stage Stage Screen]))

(defn- remove-index [xs i]
  (into (subvec xs 0 i) (subvec xs (inc i))))

(defn- ascendant-seq [^Node node]
  (cons node (lazy-seq (some-> (.getParent node) ascendant-seq))))

(defmethod event/handle ::show-popup [{:keys [index ^Event fx/event]}]
  (let [^Node source (some #(when (::result-tree-root (.getProperties ^Node %)) %) (ascendant-seq (.getSource event)))]
    #(update-in % [:result-trees index] assoc
                ::popup-window (.getWindow (.getScene source))
                ::popup-bounds (.localToScreen source (.getBoundsInLocal source)))))

(defmethod event/handle ::hide-popup [{:keys [index]}]
  #(update-in % [:result-trees index] dissoc ::popup-window ::popup-bounds))

(defn- switch-focus-on-close [state from-index]
  (-> state
      (assoc ::focus (get-in state [:result-trees (max 0 (dec from-index)) ::focus-tree/id]))
      (update ::focus-key (fnil inc 0))))

(defmethod event/handle ::close-view [{:keys [index]}]
  (fn [state]
    (let [result-trees (:result-trees state)
          tree (result-trees index)
          id (::focus-tree/id tree)
          new-tree (focus-tree/close tree)]
      (-> (if new-tree
            (assoc-in state [:result-trees index] new-tree)
            (update state :result-trees remove-index index))
          (cond-> (and (not new-tree) (< 1 (count result-trees)))
            (switch-focus-on-close index))
          (update :views dissoc id)))))

(defmethod event/handle ::close-all-views [_]
  (fn [state]
    (-> state
        (assoc :result-trees [] :views {})
        (dissoc ::focus ::focus-key))))

(defmethod event/handle ::on-view-event [{:keys [^Event fx/event index] :as e}]
  (if (and (instance? KeyEvent event)
           (= KeyEvent/KEY_PRESSED (.getEventType event)))
    (let [^KeyEvent event event
          shortcut (.isShortcutDown event)
          code (.getCode event)]
      (cond
        (and (= KeyCode/LEFT code) shortcut)
        (do
          (.consume event)
          #(update-in % [:result-trees index] focus-tree/focus-prev))

        (and (= KeyCode/RIGHT code) shortcut)
        (do
          (.consume event)
          #(update-in % [:result-trees index] focus-tree/focus-next))

        (and (= KeyCode/UP code) shortcut)
        (do (.consume event)
            (event/handle (assoc e ::event/type ::show-popup)))

        (and (= KeyCode/DOWN code) shortcut)
        (do (.consume event)
            (event/handle (assoc e ::event/type ::hide-popup)))

        (= KeyCode/ESCAPE code)
        (do
          (.consume event)
          (event/handle (assoc e ::event/type ::close-view)))

        :else
        identity))
    identity))

(defn- descendant-seq [^Node node]
  (cons node (when (instance? Parent node)
               (mapcat descendant-seq (.getChildrenUnmodifiable ^Parent node)))))

(defn focus! [node]
  (fx/run-later
    (let [^Node node (or (->> node
                              descendant-seq
                              (some #(when (.isFocusTraversable ^Node %) %)))
                         node)]
      (.requestFocus node))))

(defn focus-when-on-scene! [^Node node]
  (if (some? (.getScene node))
    (focus! node)
    (.addListener (.sceneProperty node)
                  (reify ChangeListener
                    (changed [this _ _ new-scene]
                      (when (some? new-scene)
                        (.removeListener (.sceneProperty node) this)
                        (focus! node)))))))

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

(defmethod event/handle ::focus-tab [{:keys [index view-index]}]
  #(update-in % [:result-trees index] focus-tree/focus-index view-index))

(defmethod event/handle ::select-tab [e]
  (comp (event/handle (assoc e ::event/type ::focus-tab))
        (event/handle (assoc e ::event/type ::hide-popup))))

(def ext-with-scroll-to
  (fx/make-ext-with-props
    {:scroll-to (fx.prop/make
                  (fx.mutator/setter
                    (fn [^ScrollPane pane ^Node child]
                      (fx/run-later
                        (let [viewport-b (.getViewportBounds pane)
                              pane-b (.getBoundsInLocal (.getContent pane))
                              node-b (.sceneToLocal (.getContent pane) (.localToScene child (.getBoundsInLocal child)))]
                          (cond
                            (> (.getMaxY node-b) (- (.getHeight viewport-b) (.getMinY viewport-b)))
                            (.setVvalue pane (/ (- (.getMaxY node-b) (.getHeight viewport-b))
                                                (- (.getHeight pane-b) (.getHeight viewport-b))))

                            (< (.getMinY node-b) (- (.getMinY viewport-b)))
                            (.setVvalue pane (/ (.getMinY node-b)
                                                (- (.getHeight pane-b) (.getHeight viewport-b)))))))))
                  fx.lifecycle/dynamic)}))

(defmethod event/handle ::on-popup-event [{:keys [fx/event] :as e}]
  (if (instance? KeyEvent event)
    (let [^KeyEvent event event]
      (if (= KeyEvent/KEY_PRESSED (.getEventType event))
        (do
          (.consume event)
          (condp = (.getCode event)
            KeyCode/ESCAPE (event/handle (assoc e ::event/type ::hide-popup))
            KeyCode/ENTER (event/handle (assoc e ::event/type ::hide-popup))
            KeyCode/UP (event/handle (assoc e ::event/type ::change-result-focus :fn focus-tree/focus-prev))
            KeyCode/DOWN (if (.isShortcutDown event)
                           (event/handle (assoc e ::event/type ::hide-popup))
                           (event/handle (assoc e ::event/type ::change-result-focus :fn focus-tree/focus-next)))
            KeyCode/BACK_SPACE (event/handle (assoc e ::event/type ::close-view))
            KeyCode/DELETE (event/handle (assoc e ::event/type ::close-view))
            identity))
        identity))
    identity))

(def shortcut-text
  (delay (if (.startsWith (System/getProperty "os.name") "Mac") "⌘" "Ctrl")))

(defn- result-tree-popup [{::keys [popup-window ^Bounds popup-bounds result-tree views index]}]
  (let [{::focus-tree/keys [depth id order]} result-tree]
    {:fx/type popup/view
     :bounds popup-bounds
     :window popup-window
     :on-cancel {::event/type ::hide-popup :index index}
     :position :top
     :width (- (.getWidth popup-bounds) 20)
     :desc {:fx/type fx/ext-let-refs
            :refs (into {}
                        (for [[view-index view-id] (map-indexed vector order)
                              :let [form (get-in views [view-id :form])]]
                          [view-id {:fx/type fx.h-box/lifecycle
                                    :style-class "reveal-view-result-tree-item"
                                    :pseudo-classes (if (= view-id id) #{:selected} #{})
                                    :on-mouse-entered {::event/type ::focus-tab :index index :view-index view-index}
                                    :on-mouse-clicked {::event/type ::select-tab :index index :view-index view-index}
                                    :children [{:fx/type view/summary
                                                :max-length 128
                                                :value (stream/horizontal
                                                         (stream/raw-string
                                                           (apply str (repeat (get depth view-id) "·"))
                                                           {:fill :util})
                                                         (stream/stream form))}]}]))
            :desc {:fx/type ext-with-scroll-to
                   :props {:scroll-to {:fx/type fx/ext-get-ref :ref id}}
                   :desc {:fx/type fx.scroll-pane/lifecycle
                          :event-handler {::event/type ::on-popup-event :index index}
                          :padding style/default-padding
                          :focus-traversable true
                          :style-class "reveal-view-result-tree"
                          :fit-to-width true
                          :hbar-policy :never
                          :min-height 0
                          :content {:fx/type fx.v-box/lifecycle
                                    :min-height :use-pref-size
                                    :children (for [view-id order]
                                                {:fx/type fx/ext-get-ref
                                                 :ref view-id})}}}}}))

(defn- mark-as-tree-root! [^Node node]
  (.put (.getProperties node) ::result-tree-root true))

(defn- result-tree-view [{:keys [views result-tree index]}]
  (let [{::focus-tree/keys [id depth]
         ::keys [popup-window popup-bounds]} result-tree
        {:keys [form]} (get views id)]
    {:fx/type fx/ext-let-refs
     :refs (when popup-window
             {::popup {:fx/type result-tree-popup
                       ::popup-window popup-window
                       ::popup-bounds popup-bounds
                       ::result-tree result-tree
                       ::views views
                       ::index index}})
     :desc {:fx/type fx/ext-on-instance-lifecycle
            :on-created mark-as-tree-root!
            :desc {:fx/type fx.v-box/lifecycle
                   :event-handler {::event/type ::on-view-event :index index}
                   :children [{:fx/type fx.h-box/lifecycle
                               :style-class "reveal-view-header"
                               :alignment :center
                               :children (interpose
                                           {:fx/type fx.region/lifecycle
                                            :style-class "reveal-view-header-separator"}
                                           [{:fx/type fx.button/lifecycle
                                             :style-class "reveal-view-header-button"
                                             :disable (not (focus-tree/has-prev? result-tree))
                                             :text "<"
                                             :tooltip {:fx/type fx.tooltip/lifecycle
                                                       :text (str @shortcut-text " ←")}
                                             :on-action {::event/type ::change-result-focus
                                                         :index index
                                                         :fn focus-tree/focus-prev}}
                                            {:fx/type fx.button/lifecycle
                                             :style-class "reveal-view-header-button"
                                             :disable (not (focus-tree/has-next? result-tree))
                                             :text ">"
                                             :tooltip {:fx/type fx.tooltip/lifecycle
                                                       :text (str @shortcut-text " →")}
                                             :on-action {::event/type ::change-result-focus
                                                         :index index
                                                         :fn focus-tree/focus-next}}
                                            {:fx/type fx.scroll-pane/lifecycle
                                             :h-box/hgrow :always
                                             :min-height :use-pref-size
                                             :fit-to-width true
                                             :hbar-policy :never
                                             :vbar-policy :never
                                             :tooltip {:fx/type fx.tooltip/lifecycle
                                                       :text (str @shortcut-text " ↑")}
                                             :content
                                             {:fx/type fx.button/lifecycle
                                              :style-class "reveal-view-header-button"
                                              :alignment :baseline-left
                                              :min-width 0
                                              :max-width Double/MAX_VALUE
                                              :graphic {:fx/type view/summary
                                                        :max-length 128
                                                        :value (stream/horizontal
                                                                 (stream/raw-string
                                                                   (apply str (repeat (get depth id) "·"))
                                                                   {:fill :util})
                                                                 (stream/stream form))}
                                              :on-action {::event/type ::show-popup
                                                          :index index}}}])}
                              {:fx/type ext-focused-by-default
                               :fx/key id
                               :v-box/vgrow :always
                               :desc {:fx/type fx/ext-get-ref
                                      :ref id}}]}}}))

(defmethod event/handle ::quit [_]
  (let [done (atom false)]
    (fn [state]
      (when (compare-and-set! done false true)
        ((:dispose state)))
      (dissoc state :confirm-exit-showing))))

(defmethod event/handle ::confirm-exit [{:keys [^Event fx/event close-difficulty]}]
  (.consume event)
  (if (= close-difficulty :hard)
    #(assoc % :confirm-exit-showing true)
    (event/handle {::event/type ::quit})))

(defmethod event/handle ::handle-scene-key-press [{:keys [^KeyEvent fx/event close-difficulty] :as e}]
  (cond
    (and (= KeyCode/W (.getCode event))
         (.isShortcutDown event))
    (event/handle (assoc e ::event/type ::confirm-exit))

    (and (= :easy close-difficulty)
         (= KeyCode/ESCAPE (.getCode event)))
    (do (.consume event)
        (event/handle {::event/type ::quit}))

    (or (and (= KeyCode/F11 (.getCode event))
             (not (.isAltDown event)))
        (and (= KeyCode/M (.getCode event))
             (.isShiftDown event)
             (.isShortcutDown event)))
    #(update % :maximized not)

    (or (and (= KeyCode/F11 (.getCode event))
             (.isAltDown event))
        (and (= KeyCode/M (.getCode event))
             (.isShortcutDown event)))
    #(update % :iconified not)

    :else
    identity))

(defmethod event/handle ::cancel-quit [_]
  #(dissoc % :confirm-exit-showing))

(defn- full-title [title]
  (cond-> "Reveal"
    title (str ": " title)))

(defn- short-title [title]
  (or title "Reveal"))

(defn- confirm-exit-dialog [{:keys [title]}]
  {:fx/type fx.stage/lifecycle
   :showing true
   :owner {:fx/type fx/ext-get-ref :ref ::stage}
   :on-close-request {::event/type ::cancel-quit}
   :modality :window-modal
   :title "Close window?"
   :scene {:fx/type fx.scene/lifecycle
           :stylesheets [(:cljfx.css/url @style/style)]
           :accelerators {[:escape] {::event/type ::cancel-quit}}
           :root {:fx/type fx.v-box/lifecycle
                  :style-class "reveal-ui"
                  :spacing style/default-padding
                  :padding style/default-padding
                  :children [{:fx/type fx.label/lifecycle
                              :text (str "Are you sure you want to close \"" (short-title title) "\"?")}
                             {:fx/type fx.h-box/lifecycle
                              :spacing 5
                              :alignment :center-right
                              :children [{:fx/type fx.button/lifecycle
                                          :on-action {::event/type ::cancel-quit}
                                          :text "Cancel"}
                                         {:fx/type ext-focused-by-default
                                          :desc {:fx/type fx.button/lifecycle
                                                 :on-action {::event/type ::quit}
                                                 :text "Close"}}]}]}}})

(def christmas
  (delay
    (let [now (LocalDate/now)]
      (or (.isAfter now (LocalDate/of (.getYear now) 12 20))
          (.isBefore now (LocalDate/of (.getYear now) 1 2))))))

(defn- view [{:keys [desc views result-trees] ::keys [focus focus-key]}]
  {:fx/type fx/ext-let-refs
   :refs (into {}
               (for [i (range (count result-trees))
                     id (::focus-tree/order (result-trees i))]
                 [id {:fx/type fx/ext-set-env
                      :env {::id id ::index i}
                      :desc {:fx/type view/ext-try
                             :desc (get-in views [id :desc])}}]))
   :desc {:fx/type fx/ext-let-refs
          :refs (when focus
                  {[::focus focus-key] {:fx/type ext-focused-by-default
                                        :desc {:fx/type fx/ext-get-ref
                                               :ref focus}}})
          :desc {:fx/type fx.grid-pane/lifecycle
                 :style-class "reveal-ui"
                 :column-constraints [{:fx/type fx.column-constraints/lifecycle
                                       :hgrow :always}]
                 :row-constraints (let [n (inc (count result-trees))
                                        priority 2.25
                                        total (inc (* priority (dec n)))]
                                    (->> (repeat (dec n) (* priority (/ 100 total)))
                                         (cons (/ 100 total))
                                         (map #(hash-map :fx/type fx.row-constraints/lifecycle
                                                         :percent-height %))))
                 :children
                 (into [(assoc desc
                          :grid-pane/row 0
                          :grid-pane/column 0)]
                       (map-indexed
                         (fn [i result-tree]
                           {:fx/type result-tree-view
                            :grid-pane/row (inc i)
                            :grid-pane/column 0
                            :views views
                            :index i
                            :result-tree result-tree}))
                       result-trees)}}})

(s/def ::x number?)
(s/def ::y number?)
(s/def ::width number?)
(s/def ::height number?)
(s/def ::bounds (s/coll-of (s/keys :req-un [::x ::y ::width ::height]) :kind vector?))
(s/def ::bounds.edn (s/nilable (s/map-of any? (s/map-of any? ::bounds))))

(defonce bounds-state
  (delay
    (let [state (rio/slurp-edn "bounds.edn")
          persister (agent nil
                           :error-handler (fn [_ ^Throwable ex]
                                            (.printStackTrace ex)))
          persist (fn [task state]
                    (rio/update-edn "bounds.edn" #(merge-with merge %1 %2) state)
                    task)
          schedule-persistence
          (fn [task state]
            (when task (future-cancel task))
            (.schedule event/daemon-scheduler
                       ^Runnable (fn []
                                   (send-via event/daemon-executor persister persist state))
                       1 TimeUnit/SECONDS))]
      (add-watch
        (atom
          {:state (if (s/valid? ::bounds.edn state)
                    state
                    (do
                      (println (str "Invalid " (rio/path "bounds.edn") ":"))
                      (s/explain ::bounds.edn state)
                      nil))
           :usage {}})
        ::persist
        (fn [_ _ {old :state} {new :state}]
          (when-not (= old new)
            (send-via event/daemon-executor persister schedule-persistence new)))))))

(defn- take-bounds [state scope key id default-size]
  (let [existing-bounds (get-in state [:state scope key])
        used (get-in state [:usage scope key :id] {})
        free-index (first (remove used (range (count existing-bounds))))
        index (or free-index (count existing-bounds))]
    (-> state
        (cond-> (not free-index)
          (update-in [:state scope key]
                     (fnil assoc [])
                     index
                     (let [screen-bounds (.getVisualBounds (Screen/getPrimary))
                           screen-width (.getWidth screen-bounds)
                           screen-height (.getHeight screen-bounds)
                           w (or (:width default-size) (* 0.5 screen-width))
                           h (or (:height default-size) (* 0.5 screen-height))
                           x (+ (.getMinX screen-bounds)
                                (* 0.5 (- screen-width w)))
                           y (+ (.getMinY screen-bounds)
                                (* 1/3 (- screen-height h)))]
                       {:x x :y y :width w :height h})))
        (assoc-in [:usage scope key :id index] id)
        (assoc-in [:usage scope key :index id] index))))

(defn- set-bounds [state scope key id rect]
  (let [index (get-in state [:usage scope key :index id])]
    (assoc-in state [:state scope key index] rect)))

(defn- free-bounds [state scope key id]
  (let [index (get-in state [:usage scope key :index id])]
    (-> state
        (update-in [:usage scope key :id] dissoc index)
        (update-in [:usage scope key :index] dissoc id))))

(defn- take-bounds! [scope key id default-size]
  (let [new-state (swap! @bounds-state take-bounds scope key id default-size)]
    (get-in new-state [:state scope key (get-in new-state [:usage scope key :index id])])))

(defn- set-bounds-from-ui-state [state]
  (let [{:keys [x y width height bound]} state
        {:keys [scope key id]} bound]
    (swap! @bounds-state set-bounds scope key id {:x x :y y :width width :height height}))
  state)

(defmethod event/handle ::start-window-drag [{:keys [^MouseEvent fx/event]}]
  #(assoc % :window-drag-offset-x (- (:x %) (.getScreenX event))
            :window-drag-offset-y (- (:y %) (.getScreenY event))))

(defmethod event/handle ::drag-window [{:keys [^MouseEvent fx/event]}]
  #(-> %
       (assoc :x (+ (:window-drag-offset-x %) (.getScreenX event))
              :y (+ (:window-drag-offset-y %) (.getScreenY event)))
       set-bounds-from-ui-state))

(defmethod event/handle ::start-window-resize [{:keys [^MouseEvent fx/event]}]
  #(assoc % :window-resize-offset-x (-> (:x %)
                                        (+ (:width %))
                                        (- (.getScreenX event)))
            :window-resize-offset-y (- (.getScreenY event) (:y %))))

(defmethod event/handle ::resize-window [{:keys [^MouseEvent fx/event]}]
  #(let [new-height (max 46 (-> (:y %)
                                (+ (:height %))
                                (- (.getScreenY event))
                                (+ (:window-resize-offset-y %))))]
     (-> %
         (assoc :width (max 46 (-> (:window-resize-offset-x %)
                                   (+ (.getScreenX event))
                                   (- (:x %))))
                :height new-height
                :y (-> (:y %)
                       (+ (:height %))
                       (- new-height)))
         set-bounds-from-ui-state)))

(defn- undecorated-view-wrapper [{:keys [title maximized] :as props}]
  {:fx/type fx.v-box/lifecycle
   :style-class "reveal-undecorated-wrapper"
   :children [{:fx/type fx.h-box/lifecycle
               :alignment :center
               :padding {:left 2 :right 2}
               :children (if maximized
                           [{:fx/type fx.label/lifecycle
                             :h-box/hgrow :always
                             :max-width ##Inf
                             :style-class "reveal-undecorated-title"
                             :text (short-title title)}]
                           [{:fx/type fx.label/lifecycle
                             :h-box/hgrow :always
                             :on-mouse-pressed {::event/type ::start-window-drag}
                             :on-mouse-dragged {::event/type ::drag-window}
                             :max-width ##Inf
                             :style-class "reveal-undecorated-title"
                             :pseudo-classes #{:draggable}
                             :text (short-title title)}
                            {:fx/type fx.region/lifecycle
                             :style-class "reveal-undecorated-resize"
                             :on-mouse-pressed {::event/type ::start-window-resize}
                             :on-mouse-dragged {::event/type ::resize-window}}])}
              (assoc props :fx/type view
                           :v-box/vgrow :always)]})

(defmethod event/handle ::set-bound [{:keys [key fx/event]}]
  #(cond-> % (not (:maximized %)) (-> (assoc key event) set-bounds-from-ui-state)))

(defmethod event/handle ::set-iconified [{:keys [fx/event]}]
  #(assoc % :iconified event))

(defn- window [{:keys [title showing confirm-exit-showing iconified
                       close-difficulty always-on-top maximized decorations
                       x y width height]
                :as props}]
  (let [bounds (if maximized
                 (let [b (.getVisualBounds (Screen/getPrimary))]
                   {:x (.getMinX b) :y (.getMinY b) :width (.getWidth b) :height (.getHeight b)})
                 {:x x :y y :width width :height height})
        stage {:fx/type fx.stage/lifecycle
               :style (if decorations :decorated :undecorated)
               :always-on-top always-on-top
               :iconified iconified
               :on-iconified-changed {::event/type ::set-iconified}
               :title (full-title title)
               :on-close-request {::event/type ::confirm-exit
                                  :close-difficulty close-difficulty}
               :showing showing
               :on-x-changed {::event/type ::set-bound :key :x}
               :on-y-changed {::event/type ::set-bound :key :y}
               :on-width-changed {::event/type ::set-bound :key :width}
               :on-height-changed {::event/type ::set-bound :key :height}
               :icons (if @christmas
                        ["vlaaad/reveal/logo-xmas-16.png"
                         "vlaaad/reveal/logo-xmas-32.png"
                         "vlaaad/reveal/logo-xmas-64.png"
                         "vlaaad/reveal/logo-xmas-512.png"]
                        ["vlaaad/reveal/logo-16.png"
                         "vlaaad/reveal/logo-32.png"
                         "vlaaad/reveal/logo-64.png"
                         "vlaaad/reveal/logo-256.png"
                         "vlaaad/reveal/logo-512.png"])
               :on-focused-changed {::event/type ::on-window-focused-changed}
               :scene {:fx/type fx.scene/lifecycle
                       :stylesheets [(:cljfx.css/url @style/style)]
                       :on-key-pressed {::event/type ::handle-scene-key-press
                                        :close-difficulty close-difficulty}
                       :root (assoc props :fx/type (if decorations view undecorated-view-wrapper))}}]
    {:fx/type fx/ext-let-refs
     :refs {::stage (into stage bounds)}
     :desc {:fx/type fx/ext-let-refs
            :refs (cond-> {}
                          confirm-exit-showing
                          (assoc ::confirm-exit {:fx/type confirm-exit-dialog
                                                 :title title}))
            :desc {:fx/type fx/ext-get-ref :ref ::stage}}}))

(defn oneduce
  ([xf x]
   (oneduce xf identity x))
  ([xf f x]
   (let [rf (xf (completing #(f %2)))]
     (rf (rf nil x)))))

(def ^:dynamic *eval-env*)

(defmethod event/handle ::nop [_] identity)

(def ^:private nop-event {::event/type ::nop})

(defmethod event/handle ::all [{:keys [commands]}]
  (reduce
    #(comp
       (event/handle
         (if (::event/type %2) %2 nop-event))
       %1)
    identity
    commands))

(defn command? [x]
  (try
    (not= (:vlaaad.reveal/command x ::not-found) ::not-found)
    ;; sorted maps with non-keyword keys throw class cast exceptions
    (catch Exception _ false)))

(defn- nop [])

(defonce all-windows
  (atom #{}))

(defmethod event/handle ::minimize [_]
  #(assoc % :iconified true))

(defmethod event/handle ::restore [_]
  #(assoc % :iconified false))

(defmethod event/handle ::toggle-minimized [_]
  #(update % :iconified not))

(defn make
  ([] (make {}))
  ([k v & kvs] (make (apply hash-map k v kvs)))
  ([{:keys [title value close-difficulty always-on-top bounds ;; and :decorations
            default-size dispose]
     :or {dispose nop
          close-difficulty :hard ;; :easy :normal
          always-on-top false
          bounds :default}
     :as opts}]
   (let [decorations (:decorations opts (not always-on-top))
         queryable-opts (-> opts
                            (select-keys [:title :close-difficulty :always-on-top :bounds])
                            (assoc :decorations decorations))
         desc (view/->desc value)
         dispose-delay-ref (volatile! nil)
         dispose-fn #(do @@dispose-delay-ref nil)
         bound-id (gensym "bound")
         bound-scope (System/getProperty "user.dir")
         bound-rect (take-bounds! bound-scope bounds bound-id default-size)
         *state (atom (into {:desc desc
                             :views {}
                             :result-trees []
                             :title title
                             :showing true
                             :close-difficulty close-difficulty
                             :always-on-top always-on-top
                             :maximized false
                             :iconified false
                             :decorations decorations
                             :dispose dispose-fn
                             :bound {:scope bound-scope
                                     :key bounds
                                     :id bound-id}}
                            bound-rect))
         event-handler (event/->MapEventHandler *state)
         renderer (fx/create-renderer
                    :opts {:fx.opt/map-event-handler event-handler}
                    :middleware (fx/wrap-map-desc #'window))
         process (fn process [x]
                   (if (command? x)
                     (let [form (:vlaaad.reveal/command x)]
                       (if (= :vlaaad.reveal.command/event form)
                         x
                         (let [{:keys [ns env]
                                :or {ns 'vlaaad.reveal}} x]
                           (binding [*ns* (or (when (instance? Namespace ns) ns)
                                              (find-ns ns)
                                              (do (require ns) (find-ns ns)))
                                     *eval-env* env]
                             (process (eval `(let [~@(->> env
                                                          keys
                                                          (mapcat
                                                            (fn [sym]
                                                              [sym `(get *eval-env* '~sym)])))]
                                               ~form)))))))
                     nop-event))
         ret {:dispose dispose-fn
              :execute (comp event-handler process)
              :opts queryable-opts}]
     (swap! all-windows conj ret)
     (fx/mount-renderer *state renderer)
     (vreset! dispose-delay-ref
              (delay
                (swap! all-windows disj ret)
                (swap! @bounds-state free-bounds bound-scope bounds bound-id)
                (fx/unmount-renderer *state renderer)
                (dispose)))
     ret)))

(defn- stop-queue [_ ^ArrayBlockingQueue queue]
  (.clear queue)
  false)

(defn- put-on-queue [^ArrayBlockingQueue queue x]
  (.put queue ({nil ::view/nil} x x)))

(defn- when-running [running f & args]
  (when running
    (apply f args))
  running)

(defmethod event/handle ::submit [{:keys [value]}]
  (let [done (atom false)]
    (fn [state]
      (when (compare-and-set! done false true)
        (when-let [q (-> state :desc :queue)]
          (put-on-queue q value)))
      state)))

(defn- execute-or-submit [execute x]
  (execute
    (if (command? x)
      x
      {:vlaaad.reveal/command :vlaaad.reveal.command/event
       ::event/type ::submit
       :value x})))

(defn make-queue
  ([] (make-queue {}))
  ([k v & kvs] (make-queue (apply hash-map k v kvs)))
  ([{:as opts}]
   (let [value-queue (ArrayBlockingQueue. 1024)
         *running (agent
                    true
                    :error-handler
                    (fn [a ex]
                      (send-via event/daemon-executor a when-running put-on-queue value-queue ex)))
         ui (make (assoc opts
                    :value {:fx/type view/queue
                            :queue value-queue
                            :id :output}
                    :dispose #(send-via event/daemon-executor *running stop-queue value-queue)))
         {:keys [execute dispose]} ui]
     (fn
       ([] (dispose))
       ([x]
        (send-via event/daemon-executor *running when-running execute-or-submit execute x)
        x)))))

(defn inspect
  ([x]
   (inspect x {}))
  ([x k v & kvs]
   (inspect x (apply hash-map k v kvs)))
  ([x {:as opts}]
   (make (merge
           {:title "inspect"
            :close-difficulty :easy
            :always-on-top true
            :bounds `inspect}
           opts
           {:value x}))
   x))

(defn sticker [x opts]
  (make (merge {:close-difficulty :normal
                :always-on-top true
                :bounds (:title opts :default)}
               opts
               {:value x}))
  nil)

(defmethod event/handle ::view [{:keys [value
                                        form
                                        target

                                        view-index
                                        view-id]}]
  (let [form (or form (stream/horizontal
                        (stream/raw-string "(" {:fill :util})
                        (stream/raw-string "view" {:fill :symbol})
                        stream/separator
                        (stream/stream value)
                        (stream/raw-string ")" {:fill :util})))
        done (atom false)]
    (fn [state]
      (let [target (or target
                       (when (and (:always-on-top state) (not (:maximized state)))
                         :inspector))]
        (if (= :inspector target)
          (do
            (when (compare-and-set! done false true)
              (inspect value {:title (stream/str-summary form)}))
            state)
          (let [id (UUID/randomUUID)
                desc (view/->desc value)
                result-trees (:result-trees state)
                index (if (= :new-result-panel target) (count result-trees) (or view-index 0))]
            (-> state
                (assoc :result-trees (update result-trees index focus-tree/add view-id id))
                (assoc-in [:views id] {:form form :desc desc}))))))))

(defmethod event/handle ::execute-action [{:keys [action] :as event}]
  (if (::ignore-action-result (meta (:invoke action)))
    (do
      (event/daemon-future ((:invoke action)))
      identity)
    (event/handle
      (assoc event
        ::event/type ::view
        :value {:fx/type view/derefable :derefable (event/daemon-future ((:invoke action)))}
        :form (:form action)))))

(defn- subscribe-tap [notify]
  (add-tap notify)
  #(remove-tap notify))

(defn tap-log [{:as opts}]
  (make (merge
          {:title "tap log"
           :close-difficulty :normal
           :always-on-top true
           :bounds `tap-log}
          opts
          {:value {:fx/type view/ref-watch-all
                   :result-factory (view/str-result-factory "tap>")
                   :subscribe subscribe-tap
                   :id :output}}))
  nil)

(defn submit! [pred command]
   (doseq [{:keys [execute opts]} @all-windows
           :when (pred opts)]
     (execute command)))

(defn- read-inspect-tag [form]
  `(inspect ~form :title (pr-str '~form)))