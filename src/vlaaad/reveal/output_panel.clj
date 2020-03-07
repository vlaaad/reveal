(ns vlaaad.reveal.output-panel
  (:require [vlaaad.reveal.event :as event]
            [vlaaad.reveal.layout :as layout]
            [vlaaad.reveal.action :as action]
            [vlaaad.reveal.popup :as popup]
            [cljfx.api :as fx]
            [vlaaad.reveal.canvas :as canvas])
  (:import [javafx.scene.input ScrollEvent KeyEvent MouseEvent MouseButton KeyCode Clipboard ClipboardContent]
           [javafx.scene.canvas Canvas]
           [javafx.event Event]))

(defn- state-fx [state id f & args]
  {:state (apply update state id f args)})

(defn- on-scroll [{:keys [state id ^ScrollEvent fx/event]}]
  (state-fx state id update :layout layout/scroll-by (.getDeltaX event) (.getDeltaY event)))

(defn- on-width-changed [{:keys [state id fx/event]}]
  (state-fx state id update :layout layout/set-canvas-width event))

(defn- on-height-changed [{:keys [state id fx/event]}]
  (state-fx state id update :layout layout/set-canvas-height event))

(defn add-lines [this lines]
  (update this :layout layout/add-lines lines))

(defn clear-lines [this]
  (update this :layout layout/clear-lines))

(defn- on-mouse-released [{:keys [state id]}]
  (state-fx state id update :layout layout/stop-gesture))

(defn- on-focus-changed [{:keys [state id fx/event]}]
  (when (contains? state id)
    (state-fx state id update :layout layout/set-focused event)))

(defn- on-mouse-dragged [{:keys [state id fx/event]}]
  (state-fx state id update :layout layout/perform-drag event))

(defn- show-popup [this ^Event event]
  (let [layout (layout/ensure-cursor-visible (:layout this))
        {:keys [lines cursor]} layout
        region (get-in lines cursor)
        actions (action/collect (:values region))
        bounds (layout/cursor->canvas-bounds layout)
        ^Canvas target (.getTarget event)
        screen-bounds (.localToScreen target bounds)]
    (-> this
        (assoc :layout layout)
        (cond-> actions
                (assoc :popup (assoc actions :bounds screen-bounds :segments (:segments region)))))))

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

(defn- on-mouse-pressed [{:keys [state id fx/event]}]
  (fx/run-later
    (.requestFocus ^Canvas (.getTarget event)))
  (state-fx state id handle-mouse-pressed event))

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

(defn- on-key-pressed [{:keys [state id fx/event]}]
  (state-fx state id handle-key-pressed event))

(defn- hide-popup [{:keys [state id]}]
  (state-fx state id dissoc :popup))

(defn view [{:keys [layout popup id]}]
  (let [{:keys [canvas-width canvas-height document-width document-height]} layout]
    (cond-> {:fx/type canvas/view
             :draw [layout/draw layout]
             :width canvas-width
             :height canvas-height
             :pref-width document-width
             :pref-height document-height
             :focus-traversable true
             :on-focused-changed {::event/handler on-focus-changed :id id :fx/sync true}
             :on-key-pressed {::event/handler on-key-pressed :id id :fx/sync true}
             #_#_:on-key-typed #(when-not (or (.isShortcutDown %)
                                              (.isAltDown %))
                                  (tap> {:char (.getCharacter %)
                                         :control? (Character/isISOControl (.charAt (.getCharacter %) 0))}))
             :on-mouse-dragged {::event/handler on-mouse-dragged :id id}
             :on-mouse-pressed {::event/handler on-mouse-pressed :id id :fx/sync true}
             :on-mouse-released {::event/handler on-mouse-released :id id}
             :on-width-changed {::event/handler on-width-changed :id id}
             :on-height-changed {::event/handler on-height-changed :id id}
             :on-scroll {::event/handler on-scroll :id id}}

            popup
            (assoc :popup (assoc popup :fx/type popup/view
                                       :id id
                                       :on-cancel {::event/handler hide-popup :id id})))))

(defn make []
  {:layout (layout/make)})

(defn on-add-lines [{:keys [state id fx/event]}]
  (when (contains? state id)
    (state-fx state id add-lines event)))

(defn on-clear-lines [{:keys [state id]}]
  (when (contains? state id)
    (state-fx state id clear-lines)))