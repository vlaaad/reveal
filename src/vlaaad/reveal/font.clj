(ns vlaaad.reveal.font
  (:require [clojure.java.io :as io])
  (:import [com.sun.javafx.tk Toolkit]
           [com.sun.javafx.font PGFont FontResource]
           [com.sun.javafx.geom.transform BaseTransform]))

(set! *warn-on-reflection* true)

(set! *unchecked-math* :warn-on-boxed)

(deftype Font [^javafx.scene.text.Font font ^double line-height ^double ascent char-width-cache])

(def ^:private ^:const min-cached-char-width
  -42)

(def ^:private ^:const max-cached-char-width
  Double/MAX_VALUE)

(defmacro ^:private if-class [class-name then else]
  `(try
     (Class/forName ^String ~class-name)
     ~then
     (catch ClassNotFoundException _#
       ~else)))

(def get-native-font
  (if-class "com.sun.javafx.scene.text.FontHelper"
    (let [meth (-> (Class/forName "com.sun.javafx.scene.text.FontHelper")
                   (.getDeclaredMethod "getNativeFont" (into-array Class [javafx.scene.text.Font])))]
      #(.invoke meth nil (into-array Object [%])))
    (let [meth (-> (Class/forName "javafx.scene.text.Font")
                   (.getDeclaredMethod "impl_getNativeFont" (into-array Class [])))]
      #(.invoke meth % (into-array Object [])))))

(def ^javafx.scene.text.Font font
  (javafx.scene.text.Font/loadFont (io/input-stream (io/resource "FantasqueSansMono-Regular.ttf")) 14.5))

(let [metrics (.getFontMetrics (.getFontLoader (Toolkit/getToolkit)) font)]
  (def ^double ^:const line-height (Math/ceil (.getLineHeight metrics)))
  (def ^double ^:const ascent (.getAscent metrics)))

(let [strike (.getStrike ^PGFont (get-native-font font)
                         BaseTransform/IDENTITY_TRANSFORM
                         FontResource/AA_GREYSCALE)
      cache (double-array (inc (int Character/MAX_VALUE)) Double/MIN_VALUE)]
  (defn char-width ^double [^Character character]
    (let [ch (unchecked-char character)
          i (unchecked-int ch)
          cached-width (aget cache i)]
      (if (= cached-width Double/MIN_VALUE)
        (let [width (.getCharAdvance strike ch)]
          (when (and (<= min-cached-char-width width)
                     (<= width max-cached-char-width))
            (aset cache i width))
          width)
        cached-width))))