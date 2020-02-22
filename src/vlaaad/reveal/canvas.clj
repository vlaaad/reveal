(ns vlaaad.reveal.canvas
  (:require [cljfx.composite :as fx.composite]
            [cljfx.fx.canvas :as fx.canvas]
            [cljfx.mutator :as fx.mutator]
            [cljfx.lifecycle :as fx.lifecycle])
  (:import [javafx.scene.canvas Canvas]
           [javafx.stage Popup]))

(set! *warn-on-reflection* true)

(defn- make-resizable-canvas []
  (proxy [Canvas] []
    (isResizable [] true)
    (minWidth [_] 0)
    (minHeight [_] 0)
    (maxWidth [_] 4096)
    (maxHeight [_] 4096)
    (prefWidth [_]
      (let [^Canvas this this]
        (:pref-width (.getProperties this) (.getWidth this))))
    (prefHeight [_]
      (let [^Canvas this this]
        (:pref-height (.getProperties this) (.getHeight this))))
    (resize [w h]
      (let [^Canvas this this]
        (proxy-super setWidth w)
        (proxy-super setHeight h)))))

(defn- mutable-property [key]
  (fx.mutator/setter (fn [^Canvas canvas x]
                       (let [props (.getProperties canvas)]
                         (if x
                           (.put props key x)
                           (.remove props key))
                         (when-let [p (.getParent canvas)]
                           (.requestLayout p))))))

(def view
  (fx.composite/lifecycle
    {:ctor make-resizable-canvas
     :args []
     :prop-order {:draw 1}
     :props (merge fx.canvas/props
                   (fx.composite/props Canvas
                     :draw [(fx.mutator/setter
                              (fn [^Canvas canvas [f & args]]
                                (apply f (.getGraphicsContext2D canvas) args)))
                            fx.lifecycle/scalar]
                     :pref-width [(mutable-property :pref-width)
                                  fx.lifecycle/scalar
                                  :coerce double]
                     :pref-height [(mutable-property :pref-height)
                                   fx.lifecycle/scalar
                                   :coerce double]
                     :on-width-changed [:property-change-listener fx.lifecycle/change-listener]
                     :on-height-changed [:property-change-listener fx.lifecycle/change-listener]
                     :popup [(fx.mutator/adder-remover
                               (fn [^Canvas canvas ^Popup popup]
                                 (.show popup (.getWindow (.getScene canvas))))
                               (fn [_ ^Popup popup]
                                 (.hide popup)))
                             fx.lifecycle/dynamic]))}))
