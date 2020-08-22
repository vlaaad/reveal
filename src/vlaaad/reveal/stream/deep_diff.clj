(in-ns 'vlaaad.reveal.stream)

(defstream lambdaisland.deep_diff.diff.Mismatch [{:keys [- +]}]
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

(defstream lambdaisland.deep_diff.diff.Insertion [{:keys [+]}]
  (override-style
    (horizontal
      (raw-string "+")
      (stream +))
    assoc :fill style/success-color))

(defstream lambdaisland.deep_diff.diff.Deletion [{:keys [-]}]
  (override-style
    (horizontal
      (raw-string "-")
      (stream -))
    assoc :fill style/error-color))