(ns vlaaad.reveal.specs.layout
  (:require [clojure.spec.alpha :as s]))

(s/def ::values
  (s/coll-of any? :kind vector?))

(s/def ::text
  string?)

(defn- finite? [n]
  (Double/isFinite n))

(s/def ::size-dimension
  (s/with-gen (s/and number? (complement neg?) finite?)
              #(s/gen (s/double-in :min 0 :max 100000 :NaN? false :infinite? false))))

(s/def ::width ::size-dimension)

(s/def :vlaaad.reveal.style/fill any?)

(s/def ::style
  (s/keys :opt-un [:vlaaad.reveal.style/fill]))

(s/def ::segment
  (s/keys :req-un [::text ::width ::style]))

(s/def ::segments
  (s/coll-of ::segment :kind vector?))

(s/def ::index
  (s/and int? (complement neg?)))

(s/def ::region
  (s/keys :req-un [::index ::values ::segments]))

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

(s/def ::layout
  (s/keys :req-un [::canvas-width
                   ::canvas-height
                   ::lines
                   ::scroll-x
                   ::scroll-y
                   ::document-height
                   ::drawn-line-count
                   ::dropped-line-count
                   ::scroll-y-remainder]
          :opt-un [::scroll-tab-x
                   ::scroll-tab-y
                   ::cursor
                   ::anchor
                   ::align-char-index
                   ::gesture]))
