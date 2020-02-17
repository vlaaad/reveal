(ns vlaaad.reveal.writer-output-stream
  (:import [java.io OutputStream Writer]
           [java.nio.charset Charset CodingErrorAction]
           [java.nio ByteBuffer CharBuffer]))

(set! *warn-on-reflection* true)

(defn make ^OutputStream [^Writer writer]
  (let [decoder (-> (Charset/forName "UTF-8")
                    .newDecoder
                    (.onMalformedInput CodingErrorAction/REPLACE)
                    (.onUnmappableCharacter CodingErrorAction/REPLACE)
                    (.replaceWith "?"))
        decoder-in (ByteBuffer/allocate 128)
        decoder-out (CharBuffer/allocate 1024)
        flush-output! (fn []
                        (when (pos? (.position decoder-out))
                          (.write writer (.array decoder-out) 0 (.position decoder-out))
                          (.rewind decoder-out)))
        process-input! (fn [end-of-input]
                         (.flip decoder-in)
                         (loop [decoder-result (.decode decoder decoder-in decoder-out end-of-input)]
                           (cond
                             (.isOverflow decoder-result)
                             (do
                               (flush-output!)
                               (recur (.decode decoder decoder-in decoder-out end-of-input)))

                             (.isUnderflow decoder-result)
                             nil

                             :else
                             (throw (ex-info "Unexpected decoder result"
                                             {:decoder-result decoder-result}))))
                         (.compact decoder-in))
        write! (fn [^bytes bytes off len]
                 (loop [len len
                        off off]
                   (when (pos? len)
                     (let [c (Math/min ^int len (.remaining decoder-in))]
                       (.put decoder-in bytes off c)
                       (process-input! false)
                       (recur (- len c) (+ off c)))))
                 (flush-output!))]
    (proxy [OutputStream] []
      (flush []
        (flush-output!)
        (.flush writer))
      (close []
        (process-input! true)
        (flush-output!)
        (.close writer))
      (write
        ([byte-or-bytes]
         (if (int? byte-or-bytes)
           (write! (byte-array 1 (byte byte-or-bytes)) 0 1)
           (write! byte-or-bytes 0 (alength ^bytes byte-or-bytes))))
        ([bytes off len]
         (write! bytes off len))))))