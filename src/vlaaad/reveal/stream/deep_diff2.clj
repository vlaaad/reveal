(in-ns 'vlaaad.reveal.stream)

(defstream lambdaisland.deep_diff2.diff_impl.Mismatch [{:keys [- +]}]
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

(defstream lambdaisland.deep_diff2.diff_impl.Insertion [{:keys [+]}]
  (override-style
    (horizontal
      (raw-string "+")
      (stream +))
    assoc :fill style/success-color))

(defstream lambdaisland.deep_diff2.diff_impl.Deletion [{:keys [-]}]
  (override-style
    (horizontal
      (raw-string "-")
      (stream -))
    assoc :fill style/error-color))
