(ns combo-network.graph
  (:require [cljs.pprint :refer [pprint]]))

(enable-console-print!)

;; A model for social networks
;; Riitta Toivonen!, Jukka-Pekka Onnela, Jari Sarama¨ki,
;; Jo¨rkki Hyvo¨nen, Kimmo Kaski
;; Laboratory of Computational Engineering, Helsinki University of Technology

(def initial-contact-ratio 0.95)
(def secondary-contact-ratio 0.05)

(defn seed
  [init-nodes-count]
  (range 0 init-nodes-count))

;; initial-contacts (select-random seed-nodes)
;; secondary-contact (select)

(defn in? 
  "true if coll contains elm"
  [coll elm]  
  (some #(= elm %) coll))

(defn primary-contact
  [graph new-node]
  (let [edges (:edges graph)
        nodes (:nodes graph)
        contacts (concat (filter #(< (js/Math.random) initial-contact-ratio) nodes))
        contact-edges (distinct (filter
                                 (fn [edge]
                                   (or (in? contacts (first edge))
                                       (in? contacts (nth edge 1))))
                                 edges))
        contact-neighbours (concat (map #() edges))]
    (js/console.log contacts contact-edges )
    (map #(vector % new-node) contacts)))

(defn grow-step [graph]
  (let [nodes (:nodes graph)
        new-node (+ (apply max nodes) 1)
        edges (:edges graph)
        new-edges (primary-contact graph new-node)]
    (js/console.log new-edges)
    {:nodes (conj nodes new-node)
     :edges (concat edges new-edges)}))

(defn graph-size
  [graph]
  (count (:nodes graph)))

(defn grow [final-size graph]
  (if (> final-size (graph-size  graph))
    (fn [] (grow final-size (grow-step graph)))
    graph))

(defn grow-graph-to-size [graph final-size]
  (trampoline grow final-size graph))

(defn graph [init-nodes-count final-size]
  (let [seed-nodes (seed init-nodes-count)]
    (grow-graph-to-size {:nodes seed-nodes :edges '()} final-size))) 


(pprint (graph 1 5)) ;; {:nodes (4 3 2 1 0) , :edges []}
