(ns vlaaad.reveal.font
  (:require [clojure.java.io :as io]
            [vlaaad.reveal.prefs :as prefs])
  (:import [com.sun.javafx.tk Toolkit]
           [javafx.scene.text Font Text]))

(set! *warn-on-reflection* true)

(set! *unchecked-math* :warn-on-boxed)

(defn- load-default [^double size]
  (Font/loadFont (io/input-stream (io/resource "vlaaad/reveal/FantasqueSansMono-Regular.ttf")) size))

(def ^:private *font
  (delay
    (let [[kind id] (:font-family @prefs/prefs [:default])
          size (double (:font-size @prefs/prefs 14.5))]
      (case kind
        :default (load-default size)
        :system-font (Font/font id size)
        :url-string (or (Font/loadFont ^String id size) (load-default size))))))

(defn font ^Font [] @*font)

(def ^:private *line-height
  (delay
    (-> (Toolkit/getToolkit)
        .getFontLoader
        (.getFontMetrics (font))
        .getLineHeight
        Math/ceil)))

(defn line-height ^double [] @*line-height)

(def ^:private *descent
  (delay
    (-> (Toolkit/getToolkit)
        .getFontLoader
        (.getFontMetrics (font))
        .getDescent)))

(defn descent ^double [] @*descent)

(def ^:private *char-width
  (delay
    (-> (Text. "a")
        (doto (.setFont (font)))
        .getBoundsInLocal
        .getWidth)))

(defn char-width ^double [] @*char-width)