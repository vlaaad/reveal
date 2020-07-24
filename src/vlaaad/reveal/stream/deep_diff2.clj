(in-ns 'vlaaad.reveal.stream)

(defmethod emit lambdaisland.deep_diff2.diff_impl.Mismatch [{:keys [- +]}]
  (horizontal
    (raw-string "-" {:fill style/error-color})
    (stream -)
    separator
    (raw-string "+" {:fill style/success-color})
    (stream +)))

(defmethod emit lambdaisland.deep_diff2.diff_impl.Insertion [{:keys [+]}]
  (horizontal
    (raw-string "+" {:fill style/success-color})
    (stream +)))

(defmethod emit lambdaisland.deep_diff2.diff_impl.Deletion [{:keys [-]}]
  (horizontal
    (raw-string "-" {:fill style/error-color})
    (stream -)))
