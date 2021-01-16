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
            [cljfx.lifecycle :as fx.lifecycle])
  (:import [javafx.scene.input KeyEvent KeyCode]
           [javafx.scene Node Parent]
           [javafx.beans.value ChangeListener]
           [javafx.event Event]
           [java.util.concurrent ArrayBlockingQueue]
           [java.util UUID]
           [javafx.geometry Bounds]
           [javafx.scene.control ScrollPane]
           [java.time LocalDate]
           [clojure.lang Namespace]))

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
          (dissoc id)
          (update :views dissoc id)))))

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

        (and (= KeyCode/ESCAPE code)
             (not (::consumes-escape (.getProperties ^Node (.getTarget event)))))
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
                          [view-id {:fx/type :h-box
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
                   :desc {:fx/type :scroll-pane
                          :event-filter {::event/type ::on-popup-event :index index}
                          :padding style/default-padding
                          :focus-traversable true
                          :style-class "reveal-view-result-tree"
                          :fit-to-width true
                          :hbar-policy :never
                          :min-height 0
                          :content {:fx/type :v-box
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
            :desc {:fx/type :v-box
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
                                             :tooltip {:fx/type :tooltip
                                                       :text (str @shortcut-text " ←")}
                                             :on-action {::event/type ::change-result-focus
                                                         :index index
                                                         :fn focus-tree/focus-prev}}
                                            {:fx/type :button
                                             :style-class "reveal-view-header-button"
                                             :disable (not (focus-tree/has-next? result-tree))
                                             :text ">"
                                             :tooltip {:fx/type :tooltip
                                                       :text (str @shortcut-text " →")}
                                             :on-action {::event/type ::change-result-focus
                                                         :index index
                                                         :fn focus-tree/focus-next}}
                                            {:fx/type :scroll-pane
                                             :h-box/hgrow :always
                                             :min-height :use-pref-size
                                             :fit-to-width true
                                             :hbar-policy :never
                                             :vbar-policy :never
                                             :tooltip {:fx/type :tooltip
                                                       :text (str @shortcut-text " ↑")}
                                             :content
                                             {:fx/type :button
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

(defmethod event/handle ::confirm-exit [{:keys [^Event fx/event]}]
  (.consume event)
  #(assoc % :confirm-exit-showing true))

(defmethod event/handle ::cancel-quit [_]
  #(dissoc % :confirm-exit-showing))

(defmethod event/handle ::quit [_]
  (let [done (atom false)]
    (fn [state]
      (when (compare-and-set! done false true)
        ((:dispose state)))
      (dissoc state :confirm-exit-showing))))

(defn- confirm-exit-dialog [_]
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
                                                 :on-action {::event/type ::quit}
                                                 :text "Quit"}}]}]}}})

(defn- view [{:keys [title queue showing views result-trees confirm-exit-showing]
              ::keys [focus focus-key christmas]}]
  {:fx/type fx/ext-let-refs
   :refs (into {}
               (for [i (range (count result-trees))
                     id (::focus-tree/order (result-trees i))]
                 [id {:fx/type fx/ext-set-env
                      :env {::id id ::index i}
                      :desc {:fx/type view/ext-try
                             :desc (get-in views [id :desc])}}]))
   :desc {:fx/type fx/ext-let-refs
          :refs (cond-> {::stage {:fx/type :stage
                                  :title title
                                  :on-close-request {::event/type ::confirm-exit}
                                  :showing showing
                                  :width 400
                                  :height 500
                                  :icons (if christmas
                                           ["vlaaad/reveal/logo-xmas-16.png"
                                            "vlaaad/reveal/logo-xmas-32.png"
                                            "vlaaad/reveal/logo-xmas-64.png"
                                            "vlaaad/reveal/logo-xmas-256.png"
                                            "vlaaad/reveal/logo-xmas-512.png"]
                                           ["vlaaad/reveal/logo-16.png"
                                            "vlaaad/reveal/logo-32.png"
                                            "vlaaad/reveal/logo-64.png"
                                            "vlaaad/reveal/logo-256.png"
                                            "vlaaad/reveal/logo-512.png"])
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
                  confirm-exit-showing
                  (assoc ::confirm-exit {:fx/type confirm-exit-dialog})
                  focus
                  (assoc [::focus focus-key] {:fx/type ext-focused-by-default
                                              :desc {:fx/type fx/ext-get-ref
                                                     :ref focus}}))
          :desc {:fx/type fx/ext-get-ref :ref ::stage}}})

(defn oneduce
  ([xf x]
   (oneduce xf identity x))
  ([xf f x]
   (let [rf (xf (completing #(f %2)))]
     (rf (rf nil x)))))

(defmethod event/handle ::view [{:keys [value
                                        form
                                        new-result-panel

                                        view-index
                                        view-id]}]
  (let [id (UUID/randomUUID)
        desc (view/->desc value)
        form (or form (stream/horizontal
                        (stream/raw-string "(" {:fill :util})
                        (stream/raw-string "view" {:fill :symbol})
                        stream/separator
                        (stream/stream value)
                        (stream/raw-string ")" {:fill :util})))]
    (fn [state]
      (let [result-trees (:result-trees state)
            index (if new-result-panel (count result-trees) (or view-index 0))]
        (-> state
            (assoc :result-trees (update result-trees index focus-tree/add view-id id))
            (assoc-in [:views id] {:form form :desc desc}))))))

(defmethod event/handle ::execute-action [{:keys [action] :as event}]
  (event/handle
    (assoc event
      ::event/type ::view
      :value {:fx/type view/derefable :derefable (event/daemon-future ((:invoke action)))}
      :form (:form action))))

(defn- stop-queue [_ ^ArrayBlockingQueue queue]
  (.clear queue)
  false)

(defn- put-on-queue [^ArrayBlockingQueue queue x]
  (.put queue ({nil ::view/nil} x x)))

(defn- when-running [running f & args]
  (when running
    (apply f args))
  running)

(def ^:dynamic *eval-env*)

(defmethod event/handle ::submit [{:keys [value]}]
  (let [done (atom false)]
    (fn [state]
      (when (compare-and-set! done false true)
        (put-on-queue (:queue state) value))
      state)))

(defmethod event/handle ::all [{:keys [commands]}]
  (reduce
    #(comp
       (event/handle
         (if (::event/type %2)
           %2
           {::event/type ::submit :value %2}))
       %1)
    identity
    commands))

(defn make
  ([] (make {}))
  ([k v & kvs] (make (apply hash-map k v kvs)))
  ([{:keys [title]}]
   (let [value-queue (ArrayBlockingQueue. 1024)
         *running (agent true :error-handler (fn [a ex]
                                               (send-via event/daemon-executor a when-running put-on-queue value-queue ex)))
         now (LocalDate/now)
         christmas (or (.isAfter now (LocalDate/of (.getYear now) 12 20))
                       (.isBefore now (LocalDate/of (.getYear now) 1 2)))
         *state (atom {:queue value-queue
                       :views {}
                       ::christmas christmas
                       :result-trees []
                       :title (cond-> "Reveal" title (str ": " title))
                       :showing true
                       :dispose (constantly nil)})
         event-handler (event/->MapEventHandler *state)
         renderer (fx/create-renderer
                    :opts {:fx.opt/map-event-handler event-handler}
                    :middleware (fx/wrap-map-desc #'view))
         process (fn process [x]
                   (let [form (:vlaaad.reveal/command x ::not-found)]
                     (case form
                       :vlaaad.reveal.eval/event x
                       ::not-found {::event/type ::submit :value x}
                       (let [{:keys [ns env]
                              :or {ns 'vlaaad.reveal.ext}} x]
                         (binding [*ns* (or (when (instance? Namespace ns) ns)
                                            (find-ns ns)
                                            (do (require ns) (find-ns ns)))
                                   *eval-env* env]
                           (process (eval `(let [~@(->> env
                                                        keys
                                                        (mapcat
                                                          (fn [sym]
                                                            [sym `(get *eval-env* '~sym)])))]
                                             ~form))))))))
         dispose! #(do
                     (fx/unmount-renderer *state renderer)
                     (send-via event/daemon-executor *running stop-queue value-queue))]
     (fx/mount-renderer *state renderer)
     (swap! *state assoc :dispose dispose!)
     (fn
       ([]
        (dispose!)
        nil)
       ([x]
        (send-via event/daemon-executor *running when-running (comp event-handler process) x)
        x)))))