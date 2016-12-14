(ns combo-network.graph)

(defn seed
  [init-nodes-count]
  (range 0 init-nodes-count))

;; initial-contacts (select-random seed-nodes)
;; secondary-contact (select)

(defn iterable [f]
  (fn step [pred x]
    (let [y (f x)]
      (if (pred y)
        (fn [] (step pred y))
        y))))

(defn grow
  [graph]
  (let [nodes (:nodes graph)
        edges (:edges graph)]
    (js/console.log nodes)
    {:nodes (conj nodes (+ (apply max nodes) 1))
     :edges edges}))

(defn grow-graph-to-size
  [graph final-size]
  (trampoline (iterable grow) #(< final-size %) graph))

(defn graph
  [init-nodes-count final-size]
  (let [seed-nodes (seed init-nodes-count)]
    (grow-graph-to-size {:nodes seed-nodes :edges []} final-size))) 


(js/console.log (graph 1 5))
