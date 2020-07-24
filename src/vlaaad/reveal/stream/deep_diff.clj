(in-ns 'vlaaad.reveal.stream)

(defmethod emit lambdaisland.deep_diff.diff.Mismatch [{:keys [- +]}]
  (horizontal
    (raw-string "-" {:fill style/error-color})
    (stream -)
    separator
    (raw-string "+" {:fill style/success-color})
    (stream +)))

(defmethod emit lambdaisland.deep_diff.diff.Insertion [{:keys [+]}]
  (horizontal
    (raw-string "+" {:fill style/success-color})
    (stream +)))

(defmethod emit lambdaisland.deep_diff.diff.Deletion [{:keys [-]}]
  (horizontal
    (raw-string "-" {:fill style/error-color})
    (stream -)))