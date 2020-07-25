(in-ns 'vlaaad.reveal.stream)

(defmethod emit lambdaisland.deep_diff.diff.Mismatch [{:keys [- +]}]
  (horizontal
    (override-style
      (horizontal
        (raw-string "-")
        (stream -))
      assoc :fill style/error-color)
    separator
    (override-style
      (horizontal
        (raw-string "+")
        (stream +))
      assoc :fill style/success-color)))

(defmethod emit lambdaisland.deep_diff.diff.Insertion [{:keys [+]}]
  (override-style
    (horizontal
      (raw-string "+")
      (stream +))
    assoc :fill style/success-color))

(defmethod emit lambdaisland.deep_diff.diff.Deletion [{:keys [-]}]
  (override-style
    (horizontal
      (raw-string "-")
      (stream -))
    assoc :fill style/error-color))