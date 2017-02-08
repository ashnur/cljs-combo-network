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

(defonce pi Math/PI)
(defonce two-pi (* 2 pi))
(defonce delay-after-start 2000)
(defonce travel-time 600)
(defonce G (ggen/erdos-renyi 500 0.01))
(defonce message-alpha 0.22)
(defonce message-stroke-style "#123456")
(def Gjs (clj->js G)) 
(def drawn-edges (js/Array))

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

(defn touches [node]
  (fn [link]
    (or (= (aget link "source") node) (= (aget link "target") node))))

(def current-edges (.. (aget Gjs "edges") (filter (touches init-node)) (map add-directions)))
(def force-edges nil)

(def ctx nil)

(def window-dimensions (reagent/atom (clj->js {"width" (.-innerWidth js/window)
                                               "height" (.-innerHeight js/window)})))

(defn context []
  (let [canvas (js/document.querySelector "canvas")]
    (.getContext canvas "2d")))


(defn context-update [] (set! ctx (context)))

(defn draw-node [d]
  (let [x (aget d "x")
        y (aget d "y")]
  (.moveTo ctx (+ x 2.5) y)
  (.arc ctx x y 2.5 0 two-pi)))

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
  (let [wd @window-dimensions
        w (aget wd "width")
        h (aget wd "height")
        hw (/ w 2)
        hh (/ h 2)
        nodes (aget Gjs "nodes")
        canvas (.select js/d3 (js/document.querySelector "canvas"))
        ]
    (.attr canvas "width" w)
    (.attr canvas "height" h)
    (.translate ctx hw hh)
    (aset ctx "globalAlpha" message-alpha)

    (.beginPath ctx)
    (.forEach drawn-edges draw-old-message)
    (aset ctx "strokeStyle" message-stroke-style)
    (.stroke ctx)

    (.beginPath ctx)
    (.forEach current-edges draw-message)
    (.stroke ctx)

    (aset ctx "globalAlpha" 0.3)
    (.forEach nodes draw-node)
    (aset ctx "fillStyle" (.interpolateViridis js/d3 0.3))
    (.fill ctx)
    (.translate ctx (- hw) (- hh))
    ))


(def simulation
  (let [wd @window-dimensions]
    (.. js/d3 forceSimulation
        (nodes (aget Gjs "nodes"))
        (force "link" (.. js/d3 (forceLink) (distance 50) (strength 0.02)))
        (force "link" (.. js/d3 forceLink (distance 50) (strength 0.02)))
        (force "collide" (.forceCollide js/d3 5))
        (force "charge" (.. js/d3 forceManyBody (distanceMin 20) (strength -1)))
        (force "center" (.forceCenter js/d3 0 0))
        )))

(defn on-window-resize []
  (reset! window-dimensions (clj->js {"width" (.-innerWidth js/window)
                                      "height" (.-innerHeight js/window)}))
  (.restart simulation))

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


(defn on-js-reload
  []
  (set! Gjs (clj->js G)) 
  (set! drawn-edges (js/Array)) 
  (set! init-node (rand-int (aget (aget Gjs "nodes") "length")))
  (set! messages (js/Array))
  (set! current-edges (.. (aget Gjs "edges") (filter (touches init-node)) (map add-directions)))
  (set! force-edges nil)
  
  (js/setTimeout update-vis delay-after-start)
  (.restart simulation))

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
        (context-update)
        (js/setTimeout update-vis delay-after-start)
        ;; (.. simulation (nodes (aget Gjs "nodes")) (on "tick" ticked)) 
        (.on simulation "tick" ticked)
        (reset! dom-node (reagent/dom-node this)
        ))

      :reagent-render
      (fn [ ]
        @window-dimensions
        [:div.with-canvas
         ;; reagent-render is called before the compoment mounts, so
         ;; protect against the null dom-node that occurs on the first
         ;; render
         [:canvas]])})))

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





