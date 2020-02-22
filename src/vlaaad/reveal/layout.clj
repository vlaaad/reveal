(ns vlaaad.reveal.layout
  (:require [vlaaad.reveal.font :as font]
            [cljfx.coerce :as fx.coerce]
            [vlaaad.reveal.cursor :as cursor]
            [vlaaad.reveal.style :as style]
            [clojure.string :as str]
            [vlaaad.reveal.lines :as lines])
  (:import [javafx.scene.canvas GraphicsContext]
           [javafx.scene.input MouseEvent MouseButton]
           [javafx.geometry BoundingBox Bounds]))

(set! *warn-on-reflection* true)

(defn- clamp [n min-n max-n]
  (-> n
      (max min-n)
      (min max-n)))

(defn- scroll-per-pixel [document-size canvas-size scroll-tab-size]
  (let [document-range (- document-size canvas-size)
        scroll-range (- canvas-size scroll-tab-size)]
    (if (zero? scroll-range)
      ##Inf
      (/ document-range scroll-range))))

(def ^:private ^:const scroll-bar-breadth 10.0)

(def ^:private ^:const min-scroll-tab-size 30.0)

(defn- scroll-tab-size [document-size canvas-size]
  (if (< document-size min-scroll-tab-size)
    min-scroll-tab-size
    (let [visible-ratio (min 1.0 (/ canvas-size document-size))]
      (max min-scroll-tab-size (* canvas-size visible-ratio)))))

(defn region-width [region]
  (transduce (map :width) + (:segments region)))

(defn make
  ([]
   (make {}))
  ([{:keys [canvas-width canvas-height lines scroll-x scroll-y focused scrolling-enabled]
     :or {scroll-x 0
          scroll-y 0
          lines []
          focused false
          scrolling-enabled true}
     :as layout}]
   (let [line-count (count lines)
         document-height (+ (* font/line-height line-count) (if scrolling-enabled scroll-bar-breadth 0))
         canvas-height (or canvas-height document-height)
         scroll-y (clamp scroll-y (- canvas-height document-height) 0.0)
         scroll-y-remainder (rem (- scroll-y) font/line-height)
         dropped-line-count (- (long (/ scroll-y font/line-height)))
         drawn-line-count (long (min (- line-count dropped-line-count)
                                     (Math/ceil (/ (+ canvas-height scroll-y-remainder) font/line-height))))
         document-width (+ (if scrolling-enabled scroll-bar-breadth 0)
                           (transduce
                             (map #(transduce (map region-width) + (lines %)))
                             max
                             0
                             (range dropped-line-count (+ dropped-line-count drawn-line-count))))
         canvas-width (or canvas-width document-width)
         scroll-x (clamp scroll-x (- canvas-width document-width) 0.0)]
     (-> layout
         (assoc :scroll-x scroll-x
                :scrolling-enabled scrolling-enabled
                :lines lines
                :canvas-width canvas-width
                :canvas-height canvas-height
                :focused focused
                :scroll-y scroll-y
                :document-width document-width
                :document-height document-height
                :drawn-line-count drawn-line-count
                :dropped-line-count dropped-line-count
                :scroll-y-remainder scroll-y-remainder)
         (as-> $ (if (or (not scrolling-enabled) (>= canvas-width document-width))
                   (dissoc $ :scroll-tab-x)
                   (assoc $ :scroll-tab-x (let [visible-left (- scroll-x)
                                                scroll-tab-top (- canvas-height scroll-bar-breadth)
                                                scroll-tab-width (scroll-tab-size document-width canvas-width)
                                                scroll-per-pixel (scroll-per-pixel document-width canvas-width scroll-tab-width)
                                                scroll-tab-left (/ visible-left scroll-per-pixel)]
                                            {:x scroll-tab-left
                                             :y scroll-tab-top
                                             :width scroll-tab-width
                                             :height scroll-bar-breadth
                                             :scroll-per-pixel scroll-per-pixel}))))
         (as-> $ (if (or (not scrolling-enabled) (>= canvas-height document-height))
                   (dissoc $ :scroll-tab-y)
                   (assoc $ :scroll-tab-y (let [visible-top (- scroll-y)
                                                scroll-tab-left (- canvas-width scroll-bar-breadth)
                                                scroll-tab-height (scroll-tab-size document-height canvas-height)
                                                scroll-per-pixel (scroll-per-pixel document-height canvas-height scroll-tab-height)
                                                scroll-tab-top (/ visible-top scroll-per-pixel)]
                                            {:x scroll-tab-left
                                             :y scroll-tab-top
                                             :width scroll-bar-breadth
                                             :height scroll-tab-height
                                             :scroll-per-pixel scroll-per-pixel}))))))))

(defn- draw-scroll-bar [^GraphicsContext ctx active {:keys [x y width height]}]
  (doto ctx
    (.setFill (fx.coerce/color (if active "#fff6" "#eee3")))
    (.fillRoundRect x y width height scroll-bar-breadth scroll-bar-breadth)))

(defn draw [^GraphicsContext ctx layout]
  (let [{:keys [canvas-height
                canvas-width
                scroll-y-remainder
                ^long drawn-line-count
                ^long dropped-line-count
                scroll-x
                lines
                scroll-tab-x
                scroll-tab-y
                cursor
                anchor
                gesture
                focused]} layout]
    (.clearRect ctx 0 0 canvas-width canvas-height)
    (when (and cursor anchor)
      (let [from (-> anchor
                     (cursor/min cursor)
                     (cursor/max [dropped-line-count 0]))
            last-visible-line-index (max 0 (dec (+ dropped-line-count drawn-line-count)))
            to (-> anchor
                   (cursor/max cursor)
                   (cursor/min [last-visible-line-index (dec (count (lines last-visible-line-index)))]))]
        (when-not (cursor/before? to from)
          (.setFill ctx (fx.coerce/color (if focused
                                           (::style/selection-color style/style)
                                           (::style/unfocused-selection-color style/style))))
          (doseq [i (range (cursor/row from) (inc (cursor/row to)))]
            (let [line (lines i)
                  start-col (if (= i (cursor/row from))
                              (cursor/col from)
                              0)
                  end-col (if (= i (cursor/row to))
                            (cursor/col to)
                            (dec (count line)))
                  x (transduce
                      (comp
                        (take start-col)
                        (mapcat :segments)
                        (map :width))
                      +
                      scroll-x
                      line)
                  width (transduce
                          (comp
                            (drop start-col)
                            (take (inc (- end-col start-col)))
                            (mapcat :segments)
                            (map :width))
                          +
                          0
                          line)
                  y (- (* font/line-height (- i dropped-line-count))
                       scroll-y-remainder)]
              (.fillRect ctx x y width font/line-height))))))
    (dotimes [i drawn-line-count]
      (transduce (mapcat :segments)
                 (completing
                   (fn [x {:keys [text width style]}]
                     (if (< x canvas-width)
                       (let [end (+ x width)]
                         (if (<= end 0)
                           end
                           (do
                             (.setFill ctx (fx.coerce/color
                                             (if-let [fill (:fill style)]
                                               (get style/style fill fill)
                                               "#000")))
                             (.setFont ctx font/font)
                             (.fillText ctx text x (-> (* i font/line-height)
                                                       (+ font/ascent)
                                                       (- scroll-y-remainder)))
                             end)))
                       (reduced nil))))
                 scroll-x
                 (lines (+ i dropped-line-count))))
    (some->> scroll-tab-x (draw-scroll-bar ctx (= :scroll-x (:type gesture))))
    (some->> scroll-tab-y (draw-scroll-bar ctx (= :scroll-y (:type gesture))))))

(defn set-canvas-width [layout canvas-width]
  (make (assoc layout :canvas-width canvas-width)))

(defn set-canvas-height [layout canvas-height]
  (make (assoc layout :canvas-height canvas-height)))

(defn scrolled-to-bottom? [layout]
  (let [{:keys [scroll-y canvas-height document-height]} layout]
    (or (< document-height canvas-height)
        (= scroll-y (- canvas-height document-height)))))

(defn set-cursor
  "Set cursor

  - `:anchor` - either true/false, or specific cursor value
  - `:align` - whether should update align char index used for vertical navigation"
  [layout cursor & {:keys [anchor align]
                    :or {anchor true
                         align true}}]
  (-> layout
      (assoc :cursor cursor)
      (cond-> (or align (nil? (:align-char-index layout)))
              (assoc :align-char-index (:index (get-in (:lines layout) cursor)))

              (or anchor (nil? (:anchor layout)))
              (assoc :anchor (if (cursor/cursor? anchor) anchor cursor)))))

(defn remove-cursor [layout]
  (dissoc layout :cursor :anchor :align-char-index))

(defn scroll-by [layout dx dy]
  (-> layout
      (update :scroll-x + dx)
      (update :scroll-y + dy)
      make))

(defn- arrow-scroll [layout size-key]
  (* font/line-height
     (-> 5
         (min (Math/ceil (* 0.1 (/ (get layout size-key) font/line-height))))
         (max 1))))

(defn arrow-scroll-left [layout]
  (make (update layout :scroll-x + (arrow-scroll layout :canvas-width))))

(defn arrow-scroll-right [layout]
  (make (update layout :scroll-x - (arrow-scroll layout :canvas-width))))

(defn arrow-scroll-up [layout]
  (make (update layout :scroll-y + (arrow-scroll layout :canvas-height))))

(defn arrow-scroll-down [layout]
  (make (update layout :scroll-y - (arrow-scroll layout :canvas-height))))

(defn- page-scroll [layout]
  (let [{:keys [canvas-height]} layout]
    (* font/line-height
       (max 1 (Math/ceil (* 0.5 (/ canvas-height font/line-height)))))))

(defn page-scroll-up [layout]
  (make (update layout :scroll-y + (page-scroll layout))))

(defn page-scroll-down [layout]
  (make (update layout :scroll-y - (page-scroll layout))))

(defn scroll-to-top [layout]
  (make (assoc layout :scroll-y 0)))

(defn scroll-to-bottom [layout]
  (make (assoc layout :scroll-y ##-Inf)))

(defn scroll-to-left [layout]
  (make (assoc layout :scroll-x 0)))

(defn scroll-to-right [layout]
  (make (assoc layout :scroll-x ##-Inf)))

(defn add-lines [layout lines]
  (let [layout (update layout :lines into lines)]
    (if (scrolled-to-bottom? layout)
      (scroll-to-bottom layout)
      (make layout))))

(defn empty-region? [region]
  (every? #(-> % :text str/blank?) (:segments region)))

(def non-empty-region?
  (complement empty-region?))

(defn empty-line? [line]
  (every? empty-region? line))

(def non-empty-line?
  (complement empty-line?))

(defn canvas->cursor [layout x y]
  (let [{:keys [scroll-x scroll-y lines]} layout
        doc-x (- x scroll-x)
        doc-y (- y scroll-y)
        row (long (/ doc-y font/line-height))]
    (when (< -1 row (count lines))
      (let [line (lines row)
            index (first (transduce
                           (map #(region-width (line %)))
                           (completing
                             (fn [[i x] width]
                               (let [x (+ x width)]
                                 (if (< doc-x x)
                                   (reduced [i])
                                   [(inc i) x]))))
                           [0 0]
                           (range (count line))))]
        (when (and (< index (count line)) (non-empty-region? (line index)))
          [row index])))))

(defn perform-drag [layout ^MouseEvent event]
  (if-let [gesture (:gesture layout)]
    (case (:type gesture)
      :scroll-x
      (-> layout
          (assoc :scroll-x (- (* (- (.getX event) (:offset gesture))
                                 (-> layout :scroll-tab-x :scroll-per-pixel))))
          make)
      :scroll-y
      (-> layout
          (assoc :scroll-y (- (* (- (.getY event) (:offset gesture))
                                 (-> layout :scroll-tab-y :scroll-per-pixel))))
          make)
      :selection
      (if-let [cursor (canvas->cursor layout (.getX event) (.getY event))]
        (-> layout
            (set-cursor cursor :anchor false))
        layout)


      layout)
    layout))

(defn start-gesture [layout ^MouseEvent event]
  (cond
    (not= (.getButton event) MouseButton/PRIMARY)
    layout

    (some-> (:scroll-tab-y layout) :x (<= (.getX event)))
    (-> layout
        (assoc :gesture {:type :scroll-y
                         :offset (let [event-y (.getY event)
                                       {:keys [y height]} (:scroll-tab-y layout)]
                                   (if (<= y event-y (+ y height))
                                     (- event-y y)
                                     (* height 0.5)))})
        (perform-drag event))

    (some-> (:scroll-tab-x layout) :y (<= (.getY event)))
    (-> layout
        (assoc :gesture {:type :scroll-x
                         :offset (let [event-x (.getX event)
                                       {:keys [x width]} (:scroll-tab-x layout)]
                                   (if (<= x event-x (+ x width))
                                     (- event-x x)
                                     (* width 0.5)))})
        (perform-drag event))

    :else
    (if-let [cursor (canvas->cursor layout (.getX event) (.getY event))]
      (-> layout
          (assoc :gesture {:type :selection})
          (set-cursor cursor))
      layout)))

(defn stop-gesture [layout]
  (dissoc layout :gesture))

(defn set-focused [layout focused]
  (-> layout
      (assoc :focused focused)
      (cond-> (not focused) stop-gesture)))

(defn- adjust-scroll [scroll canvas-size region-start region-size]
  (let [canvas-start (- scroll)
        region-end (+ region-start region-size)
        canvas-end (+ canvas-start canvas-size)
        start (if (> region-end canvas-end)
                (- region-start (- region-end canvas-end))
                region-start)
        start (if (< start canvas-start)
                (+ start (- canvas-start start))
                start)]
    (- scroll (- region-start start))))

(defn ensure-cursor-visible [layout]
  (let [{:keys [lines cursor canvas-width canvas-height]} layout
        [row col] cursor
        line (lines row)
        region-size (region-width (line col))
        region-start (transduce (map region-width) + (subvec line 0 col))]
    (-> layout
        (update :scroll-y adjust-scroll canvas-height (* font/line-height (cursor/row cursor)) font/line-height)
        (update :scroll-x adjust-scroll canvas-width region-start region-size)
        make)))

(defn cursor->canvas-bounds ^Bounds [layout]
  (let [{:keys [lines cursor scroll-x scroll-y]} layout
        [row col] cursor
        line (lines row)]
    (BoundingBox. (+ scroll-x (transduce (map #(-> % line region-width)) + (range col)))
                  (double (+ scroll-y (* font/line-height row)))
                  (region-width (line col))
                  font/line-height)))

(defn introduce-cursor-at-bottom-of-screen [layout]
  (let [{:keys [drawn-line-count dropped-line-count lines canvas-height scroll-y-remainder]} layout
        start-row (cond-> (dec (+ dropped-line-count drawn-line-count))
                          (< canvas-height (- (* drawn-line-count font/line-height)
                                              scroll-y-remainder))
                          dec)]
    (if-let [cursor (lines/scan lines [start-row -1] dec inc non-empty-region?)]
      (-> layout
          (set-cursor cursor)
          ensure-cursor-visible)
      layout)))

(defn introduce-cursor-at-top-of-screen [layout]
  (let [{:keys [dropped-line-count lines scroll-y-remainder]} layout
        start-row (cond-> dropped-line-count
                          (not (zero? scroll-y-remainder))
                          inc)]
    (if-let [cursor (lines/scan lines [start-row -1] inc inc non-empty-region?)]
      (-> layout
          (set-cursor cursor)
          ensure-cursor-visible)
      layout)))

(defn- binary-nearest-by [f xs x]
  (let [last-i (dec (count xs))]
    (loop [low 0
           high last-i]
      (when (<= low high)
        (let [i (quot (+ low high) 2)
              n (f (xs i))]
          (cond
            (and (<= n x)
                 (or (= i last-i)
                     (< x (f (xs (inc i))))))
            i

            (< x n)
            (recur low (dec i))

            :else
            (recur (inc i) high)))))))

(defn move-cursor-vertically [layout with-anchor direction]
  (let [{:keys [cursor lines align-char-index]} layout
        row (cursor/row cursor)]
    (if-let [row (lines/scan lines row direction non-empty-line?)]
      (let [line (lines row)
            nearest-col (binary-nearest-by :index line align-char-index)
            col (or (some #(when (non-empty-region? (line %)) %)
                          (range nearest-col (count line)))
                    (some #(when (non-empty-region? (line %)) %)
                          (range (dec nearest-col) 0 -1)))
            cursor [row col]]
        (-> layout
            (set-cursor cursor :anchor with-anchor :align false)
            ensure-cursor-visible))
      layout)))

(defn select-all [layout]
  (let [{:keys [lines]} layout
        from (lines/scan lines [##-Inf ##-Inf] inc inc non-empty-region?)
        to (lines/scan lines [##Inf ##Inf] dec dec non-empty-region?)]
    (cond-> layout
            (and from to)
            (set-cursor to :anchor from))))

(defn move-cursor-horizontally [layout with-anchor direction]
  (let [{:keys [cursor lines]} layout]
    (if-let [cursor (lines/scan lines cursor direction direction non-empty-region?)]
      (-> layout
          (set-cursor cursor :anchor with-anchor)
          ensure-cursor-visible)
      layout)))

(defn cursor-to-start-of-selection [layout]
  (let [start (cursor/min (:cursor layout) (:anchor layout))]
    (-> layout
        (set-cursor start)
        ensure-cursor-visible)))

(defn cursor-to-end-of-selection [layout]
  (let [end (cursor/max (:cursor layout) (:anchor layout))]
    (-> layout
        (set-cursor end)
        ensure-cursor-visible)))

(defn cursor-to-end-of-line [layout with-anchor]
  (let [{:keys [lines cursor]} layout
        [row col] cursor
        line (lines row)]
    (if-let [new-col (some #(when (non-empty-region? (line %)) %)
                           (range (dec (count line)) (dec col) -1))]
      (let [cursor [row new-col]]
        (-> layout
            (set-cursor cursor :anchor with-anchor)
            ensure-cursor-visible))
      layout)))

(defn cursor-to-beginning-of-line [layout with-anchor]
  (let [{:keys [lines cursor]} layout
        [row col] cursor
        line (lines row)]
    (if-let [new-col (some #(when (non-empty-region? (line %)) %) (range 0 (inc col)))]
      (let [cursor [row new-col]]
        (-> layout
            (set-cursor cursor :anchor with-anchor)
            ensure-cursor-visible))
      layout)))

(defn- string-builder
  ([] (StringBuilder.))
  ([^StringBuilder ret] (.toString ret))
  ([^StringBuilder acc in] (.append acc in)))

(defn selection-as-text [layout]
  (let [{:keys [cursor anchor lines]} layout
        from (cursor/min cursor anchor)
        to (cursor/max cursor anchor)]
    (transduce
      (comp
        (interpose ::newline)
        (mapcat (fn [row]
                  (case row
                    ::newline [{:segments [{:text "\n"}]}]
                    (let [line (lines row)
                          start-col (if (= row (cursor/row from))
                                      (cursor/col from)
                                      0)
                          end-col (if (= row (cursor/row to))
                                    (cursor/col to)
                                    (dec (count line)))]
                      (subvec line start-col (inc end-col))))))
        (mapcat :segments)
        (map :text))
      string-builder
      (range (cursor/row from) (inc (cursor/row to))))))
