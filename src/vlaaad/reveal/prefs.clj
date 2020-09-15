(ns vlaaad.reveal.prefs
  (:require [clojure.spec.alpha :as s]
            [clojure.edn :as edn]
            [clojure.main :as m])
  (:import [java.net URL MalformedURLException]
           [javafx.scene.text Font]))

(s/def ::font-size
  (s/and number? pos?))

(defn- valid-url? [s]
  (try
    (URL. s) true
    (catch MalformedURLException _ false)))

(def ^:private system-font-families
  (delay (set (Font/getFamilies))))

(defn- system-font? [s]
  (contains? @system-font-families s))

(s/def ::font-family
  (s/or :url-string (s/and string? valid-url?)
        :system-font system-font?))

(s/def ::theme
  #{:dark :light})

(s/def ::prefs
  (s/keys :opt-un [::font-family ::font-size ::theme]))

(def prefs
  (delay
    (try
      (let [raw (edn/read-string (System/getProperty "vlaaad.reveal.prefs" "{}"))
            prefs (s/conform ::prefs raw)]
        (when (s/invalid? prefs)
          (throw (ex-info "Invalid prefs" (s/explain-data ::prefs raw))))
        prefs)
      (catch Exception e
        (println "Failed to read reveal prefs")
        (println (-> e Throwable->map m/ex-triage m/ex-str))))))