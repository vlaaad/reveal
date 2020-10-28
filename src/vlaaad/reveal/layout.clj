(ns vlaaad.reveal.layout
  (:require [vlaaad.reveal.font :as font]
            [vlaaad.reveal.nav :as nav]
            [cljfx.coerce :as fx.coerce]
            [vlaaad.reveal.cursor :as cursor]
            [vlaaad.reveal.style :as style]
            [vlaaad.reveal.lines :as lines]
            [clojure.spec.alpha :as s])
  (:import [javafx.scene.canvas GraphicsContext]
           [javafx.scene.input MouseEvent MouseButton]
           [javafx.geometry BoundingBox Bounds]))

(set! *warn-on-reflection* true)

(s/def :vlaaad.reveal.annotated-value/value any?)
(s/def :vlaaad.reveal.annotated-value/annotation map?)

(s/def ::annotated-value
  (s/keys :req-un [:vlaaad.reveal.annotated-value/value
                   :vlaaad.reveal.annotated-value/annotation]))

(s/def ::value (s/nilable ::annotated-value))

(s/def ::text string?)

(defn- finite? [n]
  (Double/isFinite n))

(s/def ::size-dimension
  (s/with-gen (s/and number? (complement neg?) finite?)
              #(s/gen (s/double-in :min 0 :max 100000 :NaN? false :infinite? false))))

(s/def :vlaaad.reveal.layout.style/fill any?)
(s/def :vlaaad.reveal.layout.style/selectable boolean?)

(s/def ::style
  (s/keys :opt-un [:vlaaad.reveal.layout.style/fill
                   :vlaaad.reveal.layout.style/selectable]))

(s/def ::segment
  (s/keys :req-un [::text ::style]))

(s/def ::segments
  (s/coll-of ::segment :kind vector?))

(s/def ::index
  (s/and int? (complement neg?)))

(s/def ::selectable boolean?)

(s/def ::region
  (s/keys :req-un [::index ::value ::segments ::selectable]))

(s/def ::line
  (s/coll-of ::region :kind vector?))

(s/def ::lines
  (s/coll-of ::line :kind vector?))

(s/def ::scroll
  (s/and number? (complement pos?) finite?))

(s/def ::canvas-width
  ::size-dimension)

(s/def ::canvas-height
  ::size-dimension)

(s/def ::scroll-x
  ::scroll)

(s/def ::scroll-y
  ::scroll)

(s/def ::document-height
  ::size-dimension)

(s/def ::drawn-line-count
  (s/and int? (complement neg?)))

(s/def ::dropped-line-count
  (s/and int? (complement neg?)))

(s/def ::scroll-y-remainder
  number?)

(s/def :vlaaad.reveal.layout.scroll-tab/x number?)

(s/def :vlaaad.reveal.layout.scroll-tab/y number?)

(s/def :vlaaad.reveal.layout.scroll-tab/width number?)

(s/def :vlaaad.reveal.layout.scroll-tab/height number?)

(s/def :vlaaad.reveal.layout.scroll-tab/scroll-per-pixel number?)

(s/def ::scroll-tab
  (s/keys :req-un [:vlaaad.reveal.layout.scroll-tab/x
                   :vlaaad.reveal.layout.scroll-tab/y
                   :vlaaad.reveal.layout.scroll-tab/width
                   :vlaaad.reveal.layout.scroll-tab/height
                   :vlaaad.reveal.layout.scroll-tab/scroll-per-pixel]))

(s/def ::scroll-tab-x
  ::scroll-tab)

(s/def ::scroll-tab-y
  ::scroll-tab)

(s/def ::cursor (s/tuple ::index ::index))

(s/def ::anchor ::cursor)

(s/def ::align-char-index ::index)

(s/def ::offset (s/and number? finite?))

(defmulti gesture :type)

(defmethod gesture :scroll-x [_]
  (s/keys :req-un [::offset]))

(defmethod gesture :scroll-y [_]
  (s/keys :req-un [::offset]))

(s/def ::gesture
  (s/multi-spec gesture :type))

(s/def ::focused boolean?)

(s/def ::scrolling-enabled boolean?)

(s/def ::autoscroll boolean?)

(s/def ::layout
  (s/keys :req-un [::canvas-width
                   ::canvas-height
                   ::lines
                   ::scroll-x
                   ::scroll-y
                   ::document-height
                   ::drawn-line-count
                   ::dropped-line-count
                   ::scroll-y-remainder
                   ::focused
                   ::scrolling-enabled]
          :opt-un [::scroll-tab-x
                   ::scroll-tab-y
                   ::cursor
                   ::anchor
                   ::align-char-index
                   ::gesture
                   ::autoscroll]))

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

(defn- segment-width [segment]
  (* (font/char-width) (.length ^String (:text segment))))

(defn region-width [region]
  (transduce (map segment-width) + (:segments region)))

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
         line-height (font/line-height)
         document-height (+ (* line-height line-count) (if scrolling-enabled scroll-bar-breadth 0))
         canvas-height (or canvas-height document-height)
         scroll-y (clamp scroll-y (- canvas-height document-height) 0.0)
         scroll-y-remainder (rem (- scroll-y) line-height)
         dropped-line-count (- (long (/ scroll-y line-height)))
         drawn-line-count (long (min (- line-count dropped-line-count)
                                     (Math/ceil (/ (+ canvas-height scroll-y-remainder) line-height))))
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
    (.setFill (fx.coerce/color (if active @style/scroll-bar-color @style/inactive-scroll-bar-color)))
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
                focused]} layout
        line-height (font/line-height)
        descent (font/descent)]
    (.clearRect ctx 0 0 canvas-width canvas-height)
    (when focused
      (doto ctx
        (.setFill (fx.coerce/color @style/selection-color))
        (.fillRect 0 (- canvas-height 2) canvas-width 2)))
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
                                           @style/selection-color
                                           @style/unfocused-selection-color)))
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
                        (map segment-width))
                      +
                      scroll-x
                      line)
                  width (transduce
                          (comp
                            (drop start-col)
                            (take (inc (- end-col start-col)))
                            (mapcat :segments)
                            (map segment-width))
                          +
                          0
                          line)
                  y (- (* line-height (- i dropped-line-count))
                       scroll-y-remainder)]
              (.fillRect ctx x y width line-height))))))
    (dotimes [i drawn-line-count]
      (transduce (mapcat :segments)
                 (completing
                   (fn [x {:keys [text style] :as segment}]
                     (if (< x canvas-width)
                       (let [end (+ x (segment-width segment))]
                         (if (<= end 0)
                           end
                           (do
                             (.setFill ctx (fx.coerce/color (style/color (:fill style "#000"))))
                             (.setFont ctx (font/font))
                             (.fillText ctx text x (-> (* (inc i) line-height)
                                                       (- descent)
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
  (let [diff (- canvas-height (:canvas-height layout))]
    (make (-> layout
              (assoc :canvas-height canvas-height)
              (cond-> (neg? diff) (update :scroll-y + diff))))))

(defn scrolled-to-bottom? [layout]
  (let [{:keys [scroll-y canvas-height document-height]} layout]
    (or (< document-height canvas-height)
        (= scroll-y (- canvas-height document-height)))))

(defn scrolled-to-top? [layout]
  (zero? (:scroll-y layout)))

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

(defn ensure-rect-visible [layout {:keys [x y width height]}]
  (let [{:keys [canvas-width canvas-height]} layout]
    (-> layout
        (update :scroll-y adjust-scroll canvas-height y height)
        (update :scroll-x adjust-scroll canvas-width x width)
        make)))

(defn ensure-cursor-visible [layout]
  (let [{:keys [lines cursor]} layout
        [row col] cursor
        line (lines row)
        line-height (font/line-height)]
    (ensure-rect-visible layout
                         {:x (transduce (map region-width) + (subvec line 0 col))
                          :y (* line-height (cursor/row cursor))
                          :width (region-width (line col))
                          :height line-height})))

(defn set-cursor
  "Set cursor

  - `:anchor` - either true/false, or specific cursor value
  - `:align` - whether should update align char index used for vertical navigation
  - `:ensure-visible` - whether to scroll the view to ensure cursor is visible"
  [layout cursor & {:keys [anchor align ensure-visible]
                    :or {anchor true
                         align true
                         ensure-visible true}}]
  (if cursor
    (-> layout
        (assoc :cursor cursor)
        (cond->
          (or align (nil? (:align-char-index layout)))
          (assoc :align-char-index (:index (get-in (:lines layout) cursor)))

          (or anchor (nil? (:anchor layout)))
          (assoc :anchor (if (cursor/cursor? anchor) anchor cursor))

          ensure-visible
          ensure-cursor-visible))
    layout))

(defn remove-cursor [layout]
  (dissoc layout :cursor :anchor :align-char-index))

(defn scroll-by [layout dx dy]
  (-> layout
      (update :scroll-x + dx)
      (update :scroll-y + dy)
      make))

(defn scroll-to-top [layout]
  (make (assoc layout :scroll-y 0)))

(defn scroll-to-bottom [layout]
  (make (assoc layout :scroll-y ##-Inf)))

(defn scroll-to-left [layout]
  (make (assoc layout :scroll-x 0)))

(defn scroll-to-right [layout]
  (make (assoc layout :scroll-x ##-Inf)))

(defn- add-lines-with-nav [layout lines]
  (let [start-y (count (:lines layout))]
    (-> layout
        (update :lines into lines)
        (update :nav
                (fn [nav]
                  (reduce-kv
                    (fn [nav y line]
                      (reduce-kv
                        (fn [nav x region]
                          (if (:selectable region)
                            (let [{:keys [ids start-row]} (:nav region)
                                  ids (if (= ids [])
                                        (let [ids (nav/latest-ids nav)]
                                          (if (= ids [])
                                            [-1]
                                            ids))
                                        ids)
                                  id (peek ids)
                                  parent-ids (pop ids)]
                              (-> nav
                                  (nav/ensure-parents parent-ids)
                                  (cond-> (not (nav/has? nav id))
                                    ((if start-row nav/add-row nav/add-col) (peek parent-ids) id))
                                  (nav/add-cursor id [(+ start-y y) x])))
                            nav))
                        nav
                        line))
                    nav
                    lines))))))

(defn add-lines [layout lines]
  (let [layout (add-lines-with-nav layout lines)
        should-scroll (if (:autoscroll layout true)
                        (scrolled-to-bottom? layout)
                        (and (scrolled-to-bottom? layout)
                             (not (scrolled-to-top? layout))))]

    (if should-scroll
      (scroll-to-bottom layout)
      (make layout))))

(defn clear-lines [layout]
  (-> layout
      (assoc :lines [] :nav nil)
      remove-cursor
      make))

(defn non-empty-line? [line]
  (boolean (some :selectable line)))

(defn canvas->cursor [layout x y]
  (let [{:keys [scroll-x scroll-y lines]} layout
        doc-x (- x scroll-x)
        doc-y (- y scroll-y)
        row (long (/ doc-y (font/line-height)))]
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
        (when (and (< index (count line)) (:selectable (line index)))
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
      (set-cursor layout (canvas->cursor layout (.getX event) (.getY event))
                  :anchor false :ensure-visible false)

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
          (set-cursor cursor :anchor (not (.isShiftDown event))))
      layout)))

(defn stop-gesture [layout]
  (dissoc layout :gesture))

(defn set-focused [layout focused]
  (-> layout
      (assoc :focused focused)
      (cond-> (not focused) stop-gesture)))

(defn- clamped-nth [xs i]
  (xs (clamp i 0 (dec (count xs)))))

(defn- start-cursor [nav cursor]
  (let [id (nav/id nav cursor)
        start-cursor (nav/cursor nav id)]
    (when-not (= start-cursor cursor)
      start-cursor)))

(defn- grid-movement-cursor [nav cursor row-direction col-direction]
  (let [id (nav/id nav cursor)
        parent-id (nav/parent nav id)
        [row col] (nav/coordinate nav id)
        target-id (-> (nav/grid nav parent-id)
                      (clamped-nth (row-direction row))
                      (clamped-nth (col-direction col)))]
    (when-not (= id target-id)
      (nav/cursor nav target-id))))

(defn- next-line-cursor [nav cursor]
  (let [id (nav/id nav cursor)
        parent-id (nav/parent nav id)
        parent-grid (nav/grid nav parent-id)
        row ((nav/coordinate nav id) 0)]
    (when (< row (dec (count (nav/grid nav parent-id))))
      (nav/cursor nav ((parent-grid (inc row)) 0)))))

(defn- out-cursor [nav cursor]
  (loop [id (nav/id nav cursor)]
    (let [parent-id (nav/parent nav id)]
      (and parent-id
           (or (nav/cursor nav parent-id)
               (recur parent-id))))))

(defn- in-cursor [nav cursor]
  (loop [id (nav/id nav cursor)]
    (let [child-id (ffirst (nav/grid nav id))]
      (and child-id
           (or (nav/cursor nav child-id)
               (recur child-id))))))

(defn nav-cursor-up [layout with-anchor]
  (let [{:keys [cursor nav]} layout]
    (set-cursor layout (or (start-cursor nav cursor)
                           (grid-movement-cursor nav cursor dec identity))
                :anchor with-anchor)))

(defn nav-cursor-left [layout with-anchor]
  (let [{:keys [cursor nav]} layout]
    (set-cursor layout (or (start-cursor nav cursor)
                           (grid-movement-cursor nav cursor identity dec)
                           (out-cursor nav cursor))
                :anchor with-anchor)))

(defn nav-cursor-down [layout with-anchor]
  (let [{:keys [cursor nav]} layout]
    (set-cursor layout (or (grid-movement-cursor nav cursor inc identity)
                           (in-cursor nav cursor))
                :anchor with-anchor)))

(defn nav-cursor-right [layout with-anchor]
  (let [{:keys [cursor nav]} layout]
    (set-cursor layout (or (grid-movement-cursor nav cursor identity inc)
                           (in-cursor nav cursor)
                           (next-line-cursor nav cursor))
                :anchor with-anchor)))

(defn cursor->canvas-bounds ^Bounds [layout]
  (let [{:keys [lines cursor scroll-x scroll-y]} layout
        [row col] cursor
        line (lines row)
        line-height (font/line-height)]
    (BoundingBox. (+ scroll-x (transduce (map #(-> % line region-width)) + (range col)))
                  (double (+ scroll-y (* line-height row)))
                  (region-width (line col))
                  line-height)))

(defn introduce-cursor-at-bottom-of-screen [layout]
  (let [{:keys [drawn-line-count dropped-line-count lines canvas-height scroll-y-remainder]} layout
        start-row (cond-> (dec (+ dropped-line-count drawn-line-count))
                          (< canvas-height (- (* drawn-line-count (font/line-height))
                                              scroll-y-remainder))
                          dec)]
    (set-cursor layout (lines/scan lines [start-row -1] dec inc :selectable))))

(defn introduce-cursor-at-top-of-screen [layout]
  (let [{:keys [dropped-line-count lines scroll-y-remainder]} layout
        start-row (cond-> dropped-line-count
                          (not (zero? scroll-y-remainder))
                          inc)]
    (set-cursor layout (lines/scan lines [start-row -1] inc inc :selectable))))

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

(defn- vertical-move-cursor [layout row direction]
  (let [{:keys [lines align-char-index]} layout]
    (when-let [row (lines/scan lines row direction non-empty-line?)]
      (let [line (lines row)
            nearest-col (binary-nearest-by :index line align-char-index)
            col (or (some #(when (:selectable (line %)) %)
                          (range nearest-col (count line)))
                    (some #(when (:selectable (line %)) %)
                          (range (dec nearest-col) 0 -1)))]
        [row col]))))

(defn move-cursor-vertically [layout with-anchor direction]
  (let [{:keys [cursor]} layout
        row (cursor/row cursor)]
    (set-cursor layout (vertical-move-cursor layout row direction)
                :anchor with-anchor
                :align false)))

(defn move-cursor-home [layout with-anchor]
  (let [{:keys [lines]} layout]
    (set-cursor layout (lines/scan lines [##-Inf ##-Inf] inc inc :selectable)
                :anchor with-anchor)))

(defn move-cursor-end [layout with-anchor]
  (let [{:keys [lines]} layout]
    (set-cursor layout (lines/scan lines [##Inf ##Inf] dec dec :selectable)
                :anchor with-anchor)))

(defn move-by-page [layout direction with-anchor]
  (let [{:keys [canvas-height cursor]} layout
        line-height (font/line-height)
        row-delta (* (direction 0)
                     (int (* 0.75 (/ canvas-height line-height))))
        row (cursor/row cursor)
        new-cursor (vertical-move-cursor layout (+ row row-delta) direction)]
    (cond
      new-cursor
      (-> layout
          (scroll-by 0 (* line-height (- row (cursor/row new-cursor))))
          (set-cursor new-cursor :anchor with-anchor :align false))

      (= inc direction)
      (move-cursor-end layout with-anchor)

      :else
      (move-cursor-home layout with-anchor))))

(defn select-all [layout]
  (let [{:keys [lines]} layout
        from (lines/scan lines [##-Inf ##-Inf] inc inc :selectable)
        to (lines/scan lines [##Inf ##Inf] dec dec :selectable)]
    (cond-> layout
            (and from to)
            (set-cursor to :anchor from))))

(defn move-cursor-horizontally [layout with-anchor direction]
  (let [{:keys [cursor lines]} layout]
    (set-cursor layout (lines/scan lines cursor direction direction :selectable) :anchor with-anchor)))

(defn cursor-to-start-of-selection [layout]
  (set-cursor layout (cursor/min (:cursor layout) (:anchor layout))))

(defn cursor-to-end-of-selection [layout]
  (set-cursor layout (cursor/max (:cursor layout) (:anchor layout))))

(defn cursor-to-end-of-line [layout with-anchor]
  (let [{:keys [lines cursor]} layout
        [row col] cursor
        line (lines row)]
    (if-let [new-col (some #(when (:selectable (line %)) %)
                           (range (dec (count line)) (dec col) -1))]
      (set-cursor layout [row new-col] :anchor with-anchor)
      layout)))

(defn cursor-to-beginning-of-line [layout with-anchor]
  (let [{:keys [lines cursor]} layout
        [row col] cursor
        line (lines row)]
    (if-let [new-col (some #(when (:selectable (line %)) %) (range 0 (inc col)))]
      (set-cursor layout [row new-col] :anchor with-anchor)
      layout)))

(defn reset-anchor [layout]
  (set-cursor layout (:cursor layout)))

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
