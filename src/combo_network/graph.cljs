(ns combo-network.graph
  (:require [cljs.pprint :refer [pprint]]
            [clojure.set :refer [union, difference]]
            [cljs-time.core :refer [now, epoch]]
            ))

(enable-console-print!)

(defprotocol IGraph)

(defrecord Graph [nodes edges])

;; (defrecord IGraph
;;     Graph
;;   (erdos-renyi
;;     [final-node-count, edge-probability]
;;     ()))

;; (.log js/console (map->Graph {:edges "edges" :nodes "nodes"}))
