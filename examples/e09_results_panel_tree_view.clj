(ns e09-results-panel-tree-view
  (:require [vlaaad.reveal.ext :as rx]
            [cljfx.ext.tree-view :as fx.ext.tree-view]
            [cljfx.api :as fx])
  (:import [javafx.scene.input KeyEvent KeyCode]
           [javafx.scene.control TreeView]
           [javafx.event EventDispatcher]))

;; JavaFX TreeView consumes some key presses used by Reveal's results panel. If
;; you are creating custom actions that produce tree views, you might want to
;; make TreeView ignore these.

;; This function initializes tree view and reconfigures event dispatcher to
;; ignore some key presses:

(defn- init-tree-view! [^TreeView tree-view]
  (let [dispatcher (.getEventDispatcher tree-view)]
    (.setEventDispatcher tree-view
      (reify EventDispatcher
        (dispatchEvent [_ e next]
          (if (and (instance? KeyEvent e)
                   (or (and
                         ;; Ignore Ctrl+Up/Left/Right (used to navigate result panel tabs)
                         (.isShortcutDown ^KeyEvent e)
                         (#{KeyCode/UP KeyCode/LEFT KeyCode/RIGHT} (.getCode ^KeyEvent e)))
                       ;; Ignore Escape (to allow closing results panel tab) and Space (so
                       ;; it's impossible to have no rows selected in tree view, optional)
                       (#{KeyCode/ESCAPE KeyCode/SPACE} (.getCode ^KeyEvent e))))
            e
            (.dispatchEvent dispatcher e next)))))))

;; Example action creating tree view:

(rx/defaction ::example-tree-view [x]
  (when (::example-tree-view x)
    (fn []
      {:fx/type fx/ext-on-instance-lifecycle
       :on-created init-tree-view!
       :desc {:fx/type fx.ext.tree-view/with-selection-props
              ;; optional: select first item by default to always have some row selected
              :props {:selected-index 0}
              :desc {:fx/type :tree-view
                     :root {:fx/type :tree-item
                            :value "Inbox"
                            :children [{:fx/type :tree-item
                                        :value "Email 1"}]}}}})))

{::example-tree-view true}
