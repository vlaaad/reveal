(ns vlaaad.reveal.output-panel
  (:require [vlaaad.reveal.event :as event]
            [vlaaad.reveal.layout :as layout]
            [vlaaad.reveal.popup :as popup]
            [cljfx.api :as fx]
            [cljfx.lifecycle :as fx.lifecycle]
            [vlaaad.reveal.canvas :as canvas]
            [vlaaad.reveal.fx :as rfx]
            [vlaaad.reveal.cursor :as cursor]
            [vlaaad.reveal.font :as font]
            [cljfx.coerce :as fx.coerce]
            [vlaaad.reveal.style :as style])
  (:import [javafx.scene.input ScrollEvent KeyEvent MouseEvent MouseButton KeyCode Clipboard ClipboardContent]
           [javafx.scene.canvas Canvas GraphicsContext]
           [javafx.event Event]
           [javafx.scene Node]
           [javafx.beans.value ChangeListener]
           [org.apache.commons.lang3 StringUtils]
           [java.util.concurrent Semaphore]))

(defmethod event/handle ::on-scroll [{:keys [id ^ScrollEvent fx/event]}]
  #(update-in % [id :layout] layout/scroll-by (.getDeltaX event) (.getDeltaY event)))

(defmethod event/handle ::on-width-changed [{:keys [id fx/event]}]
  #(update-in % [id :layout] layout/set-canvas-width event))

(defmethod event/handle ::on-height-changed [{:keys [id fx/event]}]
  #(update-in % [id :layout] layout/set-canvas-height event))

(defn add-lines [this lines]
  (update this :layout layout/add-lines lines))

(defn clear-lines [this]
  (-> this
      (update :layout layout/clear-lines)
      (cond-> (:search this)
              (update :search assoc
                      :basis 0
                      :start 0
                      :end 0
                      :highlight nil
                      :results (sorted-set)))))

(defmethod event/handle ::on-mouse-released [{:keys [id]}]
  #(update-in % [id :layout] layout/stop-gesture))

(defn- update-if-exists [state id f & args]
  (if (contains? state id)
    (apply update state id f args)
    state))

(defmethod event/handle ::on-focus-changed [{:keys [id fx/event]}]
  #(update-if-exists % id update :layout layout/set-focused event))

(defmethod event/handle ::on-mouse-dragged [{:keys [id fx/event]}]
  #(update-in % [id :layout] layout/perform-drag event))

(defn- show-popup [this ^Event event]
  (let [layout (layout/ensure-cursor-visible (:layout this))
        {:keys [lines cursor]} layout
        region (get-in lines cursor)
        value (:value region)
        ^Node target (.getTarget event)]
    (-> this
        (assoc :layout layout)
        (cond-> value
                (assoc :popup {:bounds (.localToScreen target (layout/cursor->canvas-bounds layout))
                               :window (.getWindow (.getScene target))
                               :annotated-value value})))))

(defn- show-search [this]
  (assoc this :search {:term "" :results (sorted-set)}))

(defn- hide-search [this]
  (dissoc this :search))

(defn- handle-mouse-pressed [this ^MouseEvent event]
  (cond
    (= (.getButton event) MouseButton/SECONDARY)
    (let [layout (:layout this)]
      (if-let [cursor (layout/canvas->cursor layout (.getX event) (.getY event))]
        (-> this
            (assoc :layout (layout/set-cursor layout cursor))
            (show-popup event))
        this))

    :else
    (update this :layout layout/start-gesture event)))

(defmethod event/handle ::on-mouse-pressed [{:keys [id fx/event]}]
  (.requestFocus ^Canvas (.getTarget event))
  #(update % id handle-mouse-pressed event))

(defn- copy-selection! [layout]
  (fx/on-fx-thread
    (.setContent (Clipboard/getSystemClipboard)
                 (doto (ClipboardContent.)
                   (.putString (layout/selection-as-text layout)))))
  layout)

(defn- handle-key-pressed [this ^KeyEvent event]
  (let [code (.getCode event)
        shortcut (.isShortcutDown event)
        alt (.isAltDown event)
        with-anchor (not (.isShiftDown event))
        layout (:layout this)
        {:keys [cursor anchor]} layout]
    (condp = code
      KeyCode/ESCAPE
      (assoc this
        :layout
        (cond-> layout cursor layout/remove-cursor))

      KeyCode/UP
      (do
        (.consume event)
        (assoc this
          :layout
          (cond
            shortcut layout
            (and alt cursor) (layout/nav-cursor-up layout with-anchor)
            (not cursor) (layout/introduce-cursor-at-bottom-of-screen layout)
            (and with-anchor (not= cursor anchor)) (layout/cursor-to-start-of-selection layout)
            :else (layout/move-cursor-vertically layout with-anchor dec))))

      KeyCode/DOWN
      (do
        (.consume event)
        (assoc this
          :layout
          (cond
            shortcut layout
            (and alt cursor) (layout/nav-cursor-down layout with-anchor)
            (not cursor) (layout/introduce-cursor-at-top-of-screen layout)
            (and with-anchor (not= cursor anchor)) (layout/cursor-to-end-of-selection layout)
            :else (layout/move-cursor-vertically layout with-anchor inc))))

      KeyCode/LEFT
      (assoc this
        :layout
        (cond
          shortcut layout
          (and alt cursor) (layout/nav-cursor-left layout with-anchor)
          (not cursor) (layout/introduce-cursor-at-bottom-of-screen layout)
          (and with-anchor (not= cursor anchor)) (layout/cursor-to-start-of-selection layout)
          :else (layout/move-cursor-horizontally layout with-anchor dec)))

      KeyCode/RIGHT
      (assoc this
        :layout
        (cond
          shortcut layout
          (and alt cursor) (layout/nav-cursor-right layout with-anchor)
          (not cursor) (layout/introduce-cursor-at-bottom-of-screen layout)
          (and with-anchor (not= cursor anchor)) (layout/cursor-to-end-of-selection layout)
          :else (layout/move-cursor-horizontally layout with-anchor inc)))

      KeyCode/PAGE_UP
      (assoc this :layout (layout/page-scroll-up layout))

      KeyCode/PAGE_DOWN
      (assoc this :layout (layout/page-scroll-down layout))

      KeyCode/HOME
      (assoc this
        :layout
        (cond
          shortcut (-> layout
                       layout/scroll-to-top
                       (cond-> cursor layout/remove-cursor))
          (not cursor) (layout/scroll-to-left layout)
          :else (layout/cursor-to-beginning-of-line layout with-anchor)))

      KeyCode/END
      (assoc this
        :layout
        (cond
          shortcut (-> layout
                       layout/scroll-to-bottom
                       (cond-> cursor layout/remove-cursor))
          (not cursor) (layout/scroll-to-right layout)
          :else (layout/cursor-to-end-of-line layout with-anchor)))

      KeyCode/C
      (assoc this
        :layout
        (if (and (.isShortcutDown event) cursor)
          (copy-selection! layout)
          layout))

      KeyCode/A
      (assoc this
        :layout
        (if (.isShortcutDown event)
          (layout/select-all layout)
          layout))

      KeyCode/SPACE
      (cond-> this cursor (show-popup event))

      KeyCode/ENTER
      (cond-> this cursor (show-popup event))

      KeyCode/SLASH
      (show-search this)

      KeyCode/F
      (cond-> this shortcut show-search)

      this)))

(defmethod event/handle ::on-key-pressed [{:keys [id fx/event]}]
  #(update % id handle-key-pressed event))

(defmethod event/handle ::hide-popup [{:keys [id]}]
  #(update % id dissoc :popup))

(defn init-text-field-created! [^Node node]
  (.put (.getProperties node) :vlaaad.reveal.ui/consumes-escape true)
  (if (some? (.getScene node))
    (.requestFocus node)
    (.addListener (.sceneProperty node)
                  (reify ChangeListener
                    (changed [this _ _ new-scene]
                      (when (some? new-scene)
                        (.removeListener (.sceneProperty node) this)
                        (fx/run-later
                          (.requestFocus node))))))))

(defn- result->rect [[[line] char-index] term]
  (let [line-height (font/line-height)
        char-width (font/char-width)]
    {:x (* char-index char-width)
     :y (* line line-height)
     :width (* (count term) char-width)
     :height line-height}))

(defn- set-highlight [this highlight]
  (-> this
      (assoc-in [:search :highlight] highlight)
      (update :layout layout/ensure-rect-visible
              (result->rect highlight (-> this :search :term)))))

(defn- select-highlight [this]
  (let [result (-> this :search :highlight)]
    (-> this
      hide-search
      (cond-> result
        (update :layout layout/set-cursor (first result))))))

(defn- jump-to-prev-match [{:keys [search] :as this}]
  (let [highlight (:highlight search)
        highlight (when highlight
                    (first (rsubseq (:results search) < highlight)))]
    (cond-> this highlight (set-highlight highlight))))

(defn- jump-to-next-match [{:keys [search] :as this}]
  (let [highlight (:highlight search)
        highlight (when highlight
                    (first (subseq (:results search) > highlight)))]
    (cond-> this highlight (set-highlight highlight))))

(defmethod event/handle ::on-search-focus-changed [{:keys [id fx/event]}]
  (if-not event
    #(update % id hide-search)
    identity))

(defn- focus! [^Node node]
  (.requestFocus node))

(defn- search-event->output [^Event event]
  (->> ^Node (.getTarget event)
       ;; fixme: this is a horrible hack
       .getParent
       .getParent
       .getChildrenUnmodifiable
       ^Node (some #(when (instance? Canvas %) %))))

(defmethod event/handle ::on-search-event-filter [{:keys [id fx/event]}]
  (if (and (instance? KeyEvent event)
           (= KeyEvent/KEY_PRESSED (.getEventType ^KeyEvent event)))
    (let [^KeyEvent event event]
      (condp = (.getCode event)
        KeyCode/ESCAPE (do (focus! (search-event->output event))
                           (.consume event)
                           #(update % id hide-search))
        KeyCode/ENTER (do (let [output (search-event->output event)]
                            (fx/run-later (focus! output)))
                          #(update % id select-highlight))
        KeyCode/TAB (do (.consume event) identity)
        KeyCode/UP (do (.consume event) #(update % id jump-to-prev-match))
        KeyCode/DOWN (do (.consume event) #(update % id jump-to-next-match))
        identity))
    identity))

(defmethod event/handle ::on-search-text-changed [{:keys [id fx/event]}]
  #(assoc-in % [id :search :term] event))

(defn- search-view-impl [{:keys [term id]}]
  {:fx/type :stack-pane
   :style-class "reveal-search"
   :max-width :use-pref-size
   :max-height :use-pref-size
   :children [{:fx/type fx/ext-on-instance-lifecycle
               :on-created init-text-field-created!
               :desc {:fx/type :text-field
                      :style-class "reveal-text-field"
                      :prompt-text "Find..."
                      :event-filter {::event/type ::on-search-event-filter :id id}
                      :on-focused-changed {::event/type ::on-search-focus-changed :id id}
                      :pref-width 200
                      :text term
                      :on-text-changed {::event/type ::on-search-text-changed :id id}}}]})

(defn- string-builder
  ([] (StringBuilder.))
  ([^StringBuilder ret] (.toString ret))
  ([^StringBuilder acc in] (.append acc in)))

(defn- index-of-ignore-case [a b start]
  (let [result (StringUtils/indexOfIgnoreCase a b start)]
    (if (= result -1) nil result)))

(defn- draw-search [^GraphicsContext ctx
                    {:keys [dropped-line-count drawn-line-count scroll-x scroll-y-remainder]}
                    {:keys [results term highlight]}]
  (let [drawn-results (subseq results
                              >= [[dropped-line-count 0] 0]
                              <= [[(+ dropped-line-count drawn-line-count) 0] 0])
        char-width (font/char-width)
        width (* (count term) char-width)
        line-height (font/line-height)]
    (.setFill ctx (fx.coerce/color @style/search-shade-color))
    (doseq [[[line] char-index :as result] drawn-results
            :let [x (* char-index char-width)
                  vx (+ scroll-x x)
                  vy (- (* line-height (- line dropped-line-count))
                        scroll-y-remainder)
                  highlighted (= highlight result)]]
      (.setStroke ctx (fx.coerce/color
                        (if highlighted @style/search-color @style/search-shade-color)))
      (.strokeRect ctx vx vy width line-height)
      (when-not highlighted
        (.fillRect ctx vx vy width line-height)))))

(defn- indices-of [str sub]
  (if (= sub "")
    nil
    (->> (index-of-ignore-case str sub 0)
         (iterate #(index-of-ignore-case str sub (+ % (count sub))))
         (take-while some?))))

(defn- search-line [{:keys [layout search] :as this} row]
  (let [{:keys [term highlight]} search
        line ((:lines layout) row)
        strs (mapv #(transduce (map :text) string-builder (:segments %)) line)
        i->col (first (reduce (fn [[m i j] str]
                                [(assoc m j i) (inc i) (+ j (count str))])
                              [(sorted-map) 0 0]
                              strs))
        line-str (transduce identity string-builder strs)
        line-results (->> term
                       (indices-of line-str)
                       (map #(vector [row (val (first (rsubseq i->col <= %)))] %)))
        highlight (when-not highlight
                    (first line-results))]
    (-> this
        (update-in [:search :results] into line-results)
        (cond-> highlight (set-highlight highlight)))))

(defn- search-back [this]
  (-> this
      (search-line (dec (:start (:search this))))
      (update-in [:search :start] dec)))

(defn- search-forward [this]
  (-> this
      (search-line (:end (:search this)))
      (update-in [:search :end] inc)))

(defn- perform-search [{:keys [layout search] :as this}]
  (if search
    (cond-> this
            (< (:end search) (count (:lines layout)))
            search-forward
            (pos? (:start search))
            search-back)
    this))

(defn- init-search [this pid]
  (let [cursor (-> this :layout :cursor)
        basis (if cursor (cursor/row cursor) (+ (-> this :layout :dropped-line-count)
                                                (-> this
                                                    :layout
                                                    :drawn-line-count
                                                    (/ 2)
                                                    int)))]
    (update this :search assoc
            :pid pid
            :basis basis
            :start basis
            :end basis
            :highlight nil
            :results (sorted-set))))

(defn- can-search? [{:keys [layout search]}]
  (or (pos? (:start search))
      (< (:end search) (count (:lines layout)))))

(defn- search! [pid {:keys [id term]} {:keys [*state]}]
  (swap! *state update-if-exists id init-search pid)
  (when (seq term)
    (let [*running (atom true)
          s (Semaphore. 0)
          f (event/daemon-future
              (loop []
                (let [old-state @*state
                      this (get old-state id)]
                  (when (and @*running (= pid (:pid (:search this))))
                    (if (can-search? this)
                      (compare-and-set! *state old-state (assoc old-state id (perform-search this)))
                      (.acquire s))
                    (recur)))))
          watch-key [`search! pid]]
      (add-watch *state watch-key (fn [_ _ old new]
                                    (let [old-this (get old id)
                                          new-this (get new id)]
                                      (when (and (= pid
                                                    (:pid (:search old-this))
                                                    (:pid (:search new-this)))
                                                 (can-search? new-this)
                                                 (not (can-search? old-this)))
                                        (.release s)))))
      #(do
         (remove-watch *state watch-key)
         (reset! *running false)
         (future-cancel f)))))

(defn- search-view [{:keys [term id]}]
  {:fx/type fx/ext-let-refs
   :refs {:search {:fx/type rfx/ext-with-process
                   :start search!
                   :args {:id id :term term}
                   :desc {:fx/type fx.lifecycle/scalar}}}
   :desc {:fx/type search-view-impl
          :term term
          :id id}})

(defn- draw [ctx layout search]
  (layout/draw ctx layout)
  (when search
    (draw-search ctx layout search)))

(defn view [{:keys [layout popup id search]}]
  (let [{:keys [canvas-width canvas-height document-width document-height]} layout]
    {:fx/type fx/ext-let-refs
     :refs (when popup
             {::popup (assoc popup :fx/type popup/view
                                   :on-cancel {::event/type ::hide-popup :id id})})
     :desc {:fx/type :stack-pane
            :children (cond->
                        [{:fx/type canvas/view
                          :draw [draw layout search]
                          :width canvas-width
                          :height canvas-height
                          :pref-width document-width
                          :pref-height document-height
                          :focus-traversable true
                          :on-focused-changed {::event/type ::on-focus-changed :id id}
                          :on-key-pressed {::event/type ::on-key-pressed :id id}
                          :on-mouse-dragged {::event/type ::on-mouse-dragged :id id}
                          :on-mouse-pressed {::event/type ::on-mouse-pressed :id id}
                          :on-mouse-released {::event/type ::on-mouse-released :id id}
                          :on-width-changed {::event/type ::on-width-changed :id id}
                          :on-height-changed {::event/type ::on-height-changed :id id}
                          :on-scroll {::event/type ::on-scroll :id id}}]
                        search
                        (conj (assoc search
                                :fx/type search-view
                                :stack-pane/alignment :bottom-right
                                :id id)))}}))

(defn make
  ([]
   (make {}))
  ([layout]
   {:layout (layout/make layout)}))

(defmethod event/handle ::on-add-lines [{:keys [id fx/event]}]
  #(update-if-exists % id add-lines event))

(defmethod event/handle ::on-clear-lines [{:keys [id]}]
  #(update-if-exists % id clear-lines))