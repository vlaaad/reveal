(ns vlaaad.reveal.io
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn])
  (:import [java.io File]
           [clojure.lang LineNumberingPushbackReader]))

(def os (.toLowerCase (System/getProperty "os.name")))

(def app-data-dir
  (io/file
    (cond
      (.contains os "win") (System/getenv "APPDATA")
      (.contains os "mac") (str (System/getProperty "user.home") "/Library/Application Support")
      :else (System/getProperty "user.home"))
    ".reveal"))

(defn path [filename]
  (str (io/file app-data-dir filename)))

(defn- slurp-edn* [^File f]
  (when (.exists f)
    (try
      (with-open [reader (LineNumberingPushbackReader. (io/reader f))]
        (edn/read reader)))))

(defn slurp-edn [filename]
  (slurp-edn* (io/file app-data-dir filename)))

(defn update-edn [filename f & args]
  (let [file (io/file app-data-dir filename)]
    (io/make-parents file)
    (let [ret (apply f (slurp-edn* file) args)]
      (spit file (pr-str ret))
      ret)))