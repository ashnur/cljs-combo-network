(ns combo-network.graph-generator
  (:require [cljs.pprint :refer [pprint]]
            [clojure.set :refer [union, difference]]
            [cljs-time.core :refer [now, epoch]]
            [combo-network.graph :as graph]
            ))

(enable-console-print!)

;; (defprotocol IGraph
;;   "unsophisticated graph model"
;;   (erdos-renyi [final-node-count, edge-probability] )
;;   (barabasi-albert [init-node-count, step-node-count, final-node-count])
;;   (watts-strogatz [final-node-count, mean-degree, rewiring-probability] "mean-degree has to be an even integer"))


(defn choose? [p] (fn [] (< (rand) p))) 

(defn- cartesian-product
  "All the ways to take one item from each sequence"
  [& seqs]
  (let [v-original-seqs (vec seqs)
        step
        (fn step [v-seqs]
          (let [increment
                (fn [v-seqs]
                  (loop [i (dec (count v-seqs)), v-seqs v-seqs]
                    (if (= i -1) nil
                        (if-let [rst (next (v-seqs i))]
                          (assoc v-seqs i rst)
                          (recur (dec i) (assoc v-seqs i (v-original-seqs i)))))))]
            (when v-seqs
              (cons (map first v-seqs)
                    (lazy-seq (step (increment v-seqs)))))))]
    (when (every? seq seqs)
      (lazy-seq (step v-original-seqs)))))

(defn to-node [n] {:label (str "node " n)})
(defn to-edge [n]
  {:source (first n) :target (second n)})


(defn erdos-renyi
  [final-node-count edge-probability]
  (let [nodes (range 0 (- final-node-count 1))
        edges (map to-edge (filter (choose? edge-probability) (filter (fn [e] (not= (first e) (second e))) (cartesian-product nodes nodes))))]
    (graph/->Graph (map to-node nodes) edges)))
    ;(graph/->Graph (clj->js nodes) (clj->js edges))))

