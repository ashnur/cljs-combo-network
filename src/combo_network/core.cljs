(ns combo-network.core
  (:require-macros [reagent.ratom :refer [reaction]])
  (:require [reagent.core :as reagent :refer [atom]]
            [re-frame.core :refer [register-handler
                                   path
                                   register-sub
                                   dispatch
                                   dispatch-sync
                                   subscribe]]
            [cljsjs.d3]
            [combo-network.graph-generator :as ggen]
            [combo-network.graph :as graph]
            [cljs-time.core :refer [now, epoch]]
            ))

(def delay-after-start 2000)
(def travel-time 600)
(def G (ggen/erdos-renyi 20 0.10))
(def Gjs (clj->js G)) 

(.log js/console Gjs)

(defonce state (reagent/atom nil))

(def init-node (rand-int (aget (aget Gjs "nodes") "length")))

(def messages (js/Array))

(.push messages init-node) 

(defn add-directions [link]
  (let [to (if (> (.indexOf messages link.source) -1) link.target link.source)
        from (if (> (.indexOf messages link.source) -1) link.source link.target)
        ]
    (aset link "to" to)
    (aset link "from" from)
    link))

(defn touches
  [node]
  (fn [link]
    (or (= (aget link "source") node) (= (aget link "target") node))))

(def current-edges (.. (aget Gjs "edges") (filter (touches init-node)) (map add-directions)))

(def force-edges nil)

(def ctx nil)
(def message-alpha 0.22)
(def message-stroke-style "#123456")

(def window-dimensions (js-obj))
(aset window-dimensions "width" (aget js/window "innerWidth"))
(aset window-dimensions "height" (aget js/window "innerHeight"))

(def pi Math/PI)
(def two-pi (* 2 pi))

(defn context []
  (let [canvas (js/document.querySelector "canvas")]
    (aset canvas "width" (aget js/window "innerWidth"))
    (aset canvas "height" (aget js/window "innerHeight"))
    (.getContext canvas "2d")))

(defn on-window-resize []
  (aset window-dimensions "width" (aget js/window "innerWidth"))
  (aset window-dimensions "height" (aget js/window "innerHeight")))

(defn context-update [ ]
  (on-window-resize)
  (set! ctx (context)))

(defn draw-node [d]
  (.moveTo ctx (+ d.x 2.5) d.y)
  (.arc ctx d.x d.y 2.5 0 two-pi))

(def drawn-edges (js/Array))

(defn draw-old-message [msg]
  (let [nodes (aget Gjs "nodes")
        msg-source (aget msg "source")
        msg-target (aget msg "target")
        source (aget nodes msg-source)
        target (aget nodes msg-target)]
    ;(.log js/console msg msg-target target nodes)
    (.moveTo ctx (aget source "x") (aget source "y"))
    (.lineTo ctx (aget target "x") (aget target "y"))))

(defn draw-message
  [msg]
  (let [nodes (aget Gjs "nodes")
        source (aget nodes (aget msg "from"))
        target (aget nodes (aget msg "to"))
        src-x (aget source "x")
        src-y (aget source "y")
        p (aget msg "p")
        pn (if (nil? p) 0 p)
        v ((js/d3.interpolate source target) pn) 
        ]
    (.moveTo ctx src-x src-y)
    (.lineTo ctx (aget v "x") (aget v "y"))
    )
  )

(defn add-to-drawn
  [edge]
  (let [edge-from (aget edge "from")
        edge-to (aget edge "to")
        ]
    (.push drawn-edges edge)
    (if (= (.indexOf messages edge-from) -1) (.push messages edge-from))
    (if (= (.indexOf messages edge-to) -1) (.push messages edge-to))
    ))

(defn not-current-edge
  [fe]
  (not-any? (fn [edge] (= (aget fe "index") (aget edge "index"))) current-edges))

(defn ticked
  []
  (.clearRect ctx 0 0 (aget window-dimensions "width") (aget window-dimensions "height"))

  (aset ctx "globalAlpha" message-alpha)
  (.beginPath ctx)
  (.forEach drawn-edges draw-old-message)
  (aset ctx "strokeStyle" message-stroke-style)
  (.stroke ctx)

  (.beginPath ctx)
  (.forEach current-edges draw-message)
  (.stroke ctx)

  (.beginPath ctx)
  (.forEach (aget Gjs "nodes") draw-node)
  (aset ctx "globalAlpha" 0.3)
  (aset ctx "fillStyle" (.interpolateViridis js/d3 0.3))
  (.fill ctx)
  )


(def simulation
  
  (.. js/d3 forceSimulation
      (force "center" (.forceCenter js/d3 (/ (aget window-dimensions "width") 2) (/ (aget window-dimensions "height") 2)))
      (force "link" (.. js/d3 (forceLink) (distance 50) (strength 0.02)))
      (force "link" (.. js/d3 forceLink (distance 50) (strength 0.02)))
      (force "collide" (.forceCollide js/d3 5))
      (force "charge" (.. js/d3 forceManyBody (distanceMin 20) (strength -1)))
      ))

(defn in-the-dark
  [edge]
  (let [source (aget edge "source")
        target (aget edge "target")]
    (not= (> (.indexOf messages source) -1) (> (.indexOf messages target) -1))))

(defn move-message [progress] (fn [current-edge] (aset current-edge "p" (.easeCubic js/d3 progress))))

(defn update-vis
  []
  (let [timer-ref (atom nil)
        timer-fn (fn [elapsed]
                   (let [progress (/ elapsed travel-time)
                         t @timer-ref]
                     (if (or (nil? t) (< elapsed travel-time))
                       (do
                         (if (< (.alpha simulation) 0.1) (.restart simulation))
                         (.forEach current-edges (move-message progress)))
                       (do
                         (.stop t)
                         (set! force-edges (clj->js (filter not-current-edge force-edges)))
                         (.forEach current-edges add-to-drawn)
                         (let [extending-edges (.filter (aget Gjs "edges") in-the-dark)
                               next-edges (.map extending-edges add-directions)]
                           (set! current-edges next-edges))
                         (if (> (aget current-edges "length") 0) (update-vis))
                       ))))
        timer (js/d3.timer timer-fn)
        ]
    (reset! timer-ref timer)
    ))

(defn ui-control
  [state]
  [:div {:class "Control__ui"}
  [:button {:on-click #(reset! state state)} "restart"]])

(defn d3-canvas
  [state]
  (let [dom-node (reagent/atom nil)]
    (reagent/create-class
     {
      ;; :component-did-update
      ;; (fn [ this ]
      ;;   (draw-canvas-contents (.-firstChild @dom-node)))

      :component-did-mount
      (fn [ this ]
        (js/setTimeout update-vis delay-after-start)
        (context-update)
        (.. simulation (nodes (aget Gjs "nodes")) (on "tick" ticked))
        (reset! dom-node (reagent/dom-node this)
        ))

      :reagent-render
      (fn [ ]
        [:div.with-canvas
         ;; reagent-render is called before the compoment mounts, so
         ;; protect against the null dom-node that occurs on the first
         ;; render
         [:canvas (if-let [ node @dom-node ]
                    {:width (.-clientWidth node)
                     :height (.-clientHeight node)})]])})))

(defn graph-component
  [state]
  [d3-canvas state])

(defn control-component [state]
  [:div
   [graph-component state]
   [ui-control state]])

(reagent/render [control-component state]
                (js/document.getElementById "app"))

(.addEventListener js/window "resize" on-window-resize)





