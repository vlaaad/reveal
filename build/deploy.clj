(ns deploy
  (:require [cemerick.pomegranate.aether :as aether]
            [clojure.data.xml :as xml]))

(def artifact-id-tag :xmlns.http%3A%2F%2Fmaven.apache.org%2FPOM%2F4.0.0/artifactId)
(def group-id-tag :xmlns.http%3A%2F%2Fmaven.apache.org%2FPOM%2F4.0.0/groupId)
(def version-tag :xmlns.http%3A%2F%2Fmaven.apache.org%2FPOM%2F4.0.0/version)

(defn coordinates-from-pom [pom-str]
  (let [tmp (->> pom-str
                 xml/parse-str
                 :content
                 (remove string?)
                 (keep (fn [{:keys [tag] :as m}]
                         (when (or (= tag artifact-id-tag)
                                   (= tag group-id-tag)
                                   (= tag version-tag))
                           {(keyword (name tag)) (first (:content m))})))
                 (apply merge))]
    [(symbol (str (:groupId tmp) "/" (:artifactId tmp))) (:version tmp)]))

(defn -main [artifact username token]
  (let [coordinates (coordinates-from-pom (slurp "pom.xml"))]
    (println "Deploying" coordinates "to clojars as" username)
    (aether/deploy
      :pom-file "pom.xml"
      :jar-file artifact
      :repository {"clojars" {:url "https://clojars.org/repo"
                              :username username
                              :password token}}
      :coordinates coordinates)))

