(in-ns 'vlaaad.reveal.stream)

(import 'java.sql.Timestamp)

(def ^:private utc-timestamp-format
  (proxy [ThreadLocal] []
    (initialValue []
      (doto (SimpleDateFormat. "yyyy-MM-dd'T'HH:mm:ss")
        (.setTimeZone (TimeZone/getTimeZone "GMT"))))))

(defstream Timestamp [^Timestamp timestamp]
  (horizontal
    (raw-string "#inst" {:fill style/object-color})
    separator
    (stream (str (.format ^DateFormat (.get ^ThreadLocal utc-timestamp-format) timestamp)
                 (format ".%09d-00:00" (.getNanos timestamp))))))