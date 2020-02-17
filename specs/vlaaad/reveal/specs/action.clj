(ns vlaaad.reveal.specs.action
  (:require [clojure.spec.alpha :as s]))

(s/def ::id any?)
(s/def ::label string?)
(s/def ::check fn?)
(s/def ::action
  (s/keys :req-un [::id ::check ::label]))

(s/fdef vlaaad.reveal.action/register!
  :args (s/cat :action ::action)
  :ret ::id)
