(ns vlaaad.reveal.font
  (:require [clojure.java.io :as io]
            [vlaaad.reveal.prefs :as prefs])
  (:import [com.sun.javafx.tk Toolkit]
           [com.sun.javafx.font PGFont FontResource]
           [com.sun.javafx.geom.transform BaseTransform]
           [javafx.scene.text Font]))

(set! *warn-on-reflection* true)

(set! *unchecked-math* :warn-on-boxed)

(defmacro ^:private if-class [class-name then else]
  `(try
     (Class/forName ^String ~class-name)
     ~then
     (catch ClassNotFoundException _#
       ~else)))

(def get-native-font
  (if-class "com.sun.javafx.scene.text.FontHelper"
    (let [meth (-> (Class/forName "com.sun.javafx.scene.text.FontHelper")
                   (.getDeclaredMethod "getNativeFont" (into-array Class [Font])))]
      #(.invoke meth nil (into-array Object [%])))
    (let [meth (-> (Class/forName "javafx.scene.text.Font")
                   (.getDeclaredMethod "impl_getNativeFont" (into-array Class [])))]
      #(.invoke meth % (into-array Object [])))))

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
    (-> (font)
        ^PGFont get-native-font
        (.getStrike BaseTransform/IDENTITY_TRANSFORM FontResource/AA_GREYSCALE)
        (.getCharAdvance \a))))

(defn char-width ^double [] @*char-width)