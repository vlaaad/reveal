(ns vlaaad.reveal.output-panel
  (:require [vlaaad.reveal.event :as event]
            [vlaaad.reveal.layout :as layout]
            [vlaaad.reveal.popup :as popup]
            [cljfx.api :as fx]
            [vlaaad.reveal.canvas :as canvas])
  (:import [javafx.scene.input ScrollEvent KeyEvent MouseEvent MouseButton KeyCode Clipboard ClipboardContent]
           [javafx.scene.canvas Canvas]
           [javafx.event Event]))

(defmethod event/handle ::on-scroll [*state {:keys [id ^ScrollEvent fx/event]}]
  (swap! *state update-in [id :layout] layout/scroll-by (.getDeltaX event) (.getDeltaY event)))

(defmethod event/handle ::on-width-changed [*state {:keys [id fx/event]}]
  (swap! *state update-in [id :layout] layout/set-canvas-width event))

(defmethod event/handle ::on-height-changed [*state {:keys [id fx/event]}]
  (swap! *state update-in [id :layout] layout/set-canvas-height event))

(defn add-lines [this lines]
  (update this :layout layout/add-lines lines))

(defn clear-lines [this]
  (update this :layout layout/clear-lines))

(defmethod event/handle ::on-mouse-released [*state {:keys [id]}]
  (swap! *state update-in [id :layout] layout/stop-gesture))

(defmethod event/handle ::on-focus-changed [*state {:keys [id fx/event]}]
  (swap! *state #(cond-> %
                   (contains? % id) (update-in [id :layout] layout/set-focused event))))

(defmethod event/handle ::on-mouse-dragged [*state {:keys [id fx/event]}]
  (swap! *state update-in [id :layout] layout/perform-drag event))

(defn- show-popup [this ^Event event]
  (let [layout (layout/ensure-cursor-visible (:layout this))
        {:keys [lines cursor]} layout
        region (get-in lines cursor)
        [value annotation :as val+ann] (peek (:values region))]
    (-> this
        (assoc :layout layout)
        (cond-> val+ann
                (assoc :popup {:bounds (-> ^Canvas (.getTarget event)
                                           (.localToScreen
                                             (layout/cursor->canvas-bounds layout)))
                               :value value
                               :annotation annotation})))))

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

(defmethod event/handle ::on-mouse-pressed [*state {:keys [id fx/event]}]
  (.requestFocus ^Canvas (.getTarget event))
  (swap! *state update id handle-mouse-pressed event))

(defn- copy-selection! [layout]
  (fx/on-fx-thread
    (.setContent (Clipboard/getSystemClipboard)
                 (doto (ClipboardContent.)
                   (.putString (layout/selection-as-text layout)))))
  layout)

(defn- handle-key-pressed [this ^KeyEvent event]
  (let [code (.getCode event)
        shortcut (.isShortcutDown event)
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
            (not cursor) (layout/introduce-cursor-at-top-of-screen layout)
            (and with-anchor (not= cursor anchor)) (layout/cursor-to-end-of-selection layout)
            :else (layout/move-cursor-vertically layout with-anchor inc))))

      KeyCode/LEFT
      (assoc this
        :layout
        (cond
          shortcut layout
          (not cursor) (layout/introduce-cursor-at-bottom-of-screen layout)
          (and with-anchor (not= cursor anchor)) (layout/cursor-to-start-of-selection layout)
          :else (layout/move-cursor-horizontally layout with-anchor dec)))

      KeyCode/RIGHT
      (assoc this
        :layout
        (cond
          shortcut layout
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

      this)))

(defmethod event/handle ::on-key-pressed [*state {:keys [id fx/event]}]
  (swap! *state update id handle-key-pressed event))

(defmethod event/handle ::hide-popup [*state {:keys [id]}]
  (swap! *state update id dissoc :popup))

(defn view [{:keys [layout popup id]}]
  (let [{:keys [canvas-width canvas-height document-width document-height]} layout]
    (cond-> {:fx/type canvas/view
             :draw [layout/draw layout]
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
             :on-scroll {::event/type ::on-scroll :id id}}

            popup
            (assoc :popup (assoc popup :fx/type popup/view
                                       :on-cancel {::event/type ::hide-popup :id id})))))

(defn make []
  {:layout (layout/make)})

(defmethod event/handle ::on-add-lines [*state {:keys [id fx/event]}]
  (swap! *state #(cond-> % (contains? % id) (update id add-lines event))))

(defmethod event/handle ::on-clear-lines [*state {:keys [id]}]
  (swap! *state #(cond-> % (contains? % id) (update id clear-lines))))