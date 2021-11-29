(ns e01-loom-formatters-and-actions
  (:require [vlaaad.reveal :as r]
            [loom.graph :as g])
  (:import [loom.graph BasicEditableGraph]))

;; This example adds formatting and navigation support for loom: a clojure
;; library for building graph data structures.
;; see https://github.com/aysylu/loom

;; by default we only show graph nodes when displaying the graph:

(r/defstream BasicEditableGraph [{:keys [nodeset] :as graph}]
  (r/horizontal
    (r/raw-string "#loom/graph[" {:fill :object})
    (r/items nodeset {::graph graph})
    (r/raw-string "]" {:fill :object})))

;; define an action on a selected node when it has access to graph:

(r/defaction ::loom:successors [v ann]
  (when-let [g (::graph ann)]
    (fn []
      ;; forward the graph to every successor
      (r/vertically (g/successors g v) {::graph g}))))

;; define an action on a node to see its graph:

(r/defaction ::loom:graph [v ann]
  (when-let [g (::graph ann)]
    (constantly g)))

;; define an action on a graph to see its edges:

(r/defaction ::loom:edges [v]
  (when (satisfies? g/Graph v)
    (fn []
      (r/vertically
        (map (fn [[from to]]
               (r/horizontal
                 ;; forward the graph
                 (r/stream from {::graph v})
                 r/separator
                 (r/raw-string "->" {:fill :util :selectable false})
                 r/separator
                 (r/stream to {::graph v})))
             (g/edges v))))))

;; graph of associations to explore:

(g/graph [:food :takoyaki]
         [:food :drinks]
         [:takoyaki :japan]
         [:drinks :coca-cola]
         [:drinks :genmaicha]
         [:genmaicha :japan])