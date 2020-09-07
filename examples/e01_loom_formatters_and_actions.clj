(ns e01-loom-formatters-and-actions
  (:require [vlaaad.reveal.ext :as rx]
            [vlaaad.reveal.style :as style]
            [loom.graph :as g])
  (:import [loom.graph BasicEditableGraph]))

;; This example adds formatting and navigation support for loom: a clojure
;; library for building graph data structures.
;; see https://github.com/aysylu/loom

;; by default we only show graph nodes when displaying the graph:

(rx/defstream BasicEditableGraph [{:keys [nodeset] :as graph}]
  (rx/horizontal
    (rx/raw-string "#loom/graph[" {:fill style/object-color})
    (rx/items nodeset {::graph graph})
    (rx/raw-string "]" {:fill style/object-color})))

;; define an action on a selected node when it has access to graph:

(rx/defaction ::loom:successors [v ann]
  (when-let [g (::graph ann)]
    (fn []
      ;; we want to stream this custom formatting as is, not as a function value
      (rx/stream-as-is
        ;; forward the graph to every successor
        (rx/vertically (g/successors g v) {::graph g})))))

;; define an action on a node to see its graph:

(rx/defaction ::loom:graph [v ann]
  (when-let [g (::graph ann)]
    (constantly g)))

;; define an action on a graph to see its edges:

(rx/defaction ::loom:edges [v]
  (when (satisfies? g/Graph v)
    (fn []
      (rx/stream-as-is
        (rx/vertically
          (map (fn [[from to]]
                 ;; since vertically works on values, not sfs, we want to make these sfs
                 ;; streamed as is too
                 (rx/stream-as-is
                   (rx/horizontal
                     ;; forward the graph
                     (rx/stream from {::graph v})
                     rx/separator
                     (rx/raw-string "->" {:fill style/util-color :selectable false})
                     rx/separator
                     (rx/stream to {::graph v}))))
               (g/edges v)))))))

;; graph of associations to explore:

(g/graph [:food :takoyaki]
         [:food :drinks]
         [:takoyaki :japan]
         [:drinks :coca-cola]
         [:drinks :genmaicha]
         [:genmaicha :japan])