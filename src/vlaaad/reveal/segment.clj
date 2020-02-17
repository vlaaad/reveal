(ns vlaaad.reveal.segment
  (:require [vlaaad.reveal.canvas :as canvas]
            [vlaaad.reveal.layout :as layout]))

(defn view [{:keys [segments width on-width-changed height on-height-changed]}]
  (let [layout (layout/make {:canvas-width width
                             :canvas-height height
                             :scrolling-enabled false
                             :lines [[{:index 0
                                       :values []
                                       :segments segments}]]})
        {:keys [canvas-width canvas-height document-width document-height]} layout]
    {:fx/type canvas/view
     :draw [layout/draw layout]
     :width canvas-width
     :height canvas-height
     :pref-width document-width
     :pref-height document-height
     :on-width-changed on-width-changed
     :on-height-changed on-height-changed}))
