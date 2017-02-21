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

(defonce node-radius 2.5)

(def delay-after-start 2000)
(def travel-time 1600)

(def total-node-count 30)
(def peer-connections-count (.log js/Math total-node-count))
(def connection-average-chance (/ peer-connections-count total-node-count))

(def G (ggen/erdos-renyi total-node-count connection-average-chance))

(def old-message-alpha 0.3)
(def old-message-stroke-style "#ccc")
(def message-alpha 0.2)
(def message-stroke-style "#0fff00")

(defonce state (reagent/atom nil))

(def Gjs (clj->js G)) 
(def init-node (rand-int (aget (aget Gjs "nodes") "length")))
(def drawn-edges (js/Array))
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
(def update-last-edges true)
(def force-edges [])

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
  (.moveTo ctx (+ x node-radius) y)
  (.arc ctx x y node-radius 0 two-pi)))

(defn draw-old-message [msg]
  (let [nodes (aget Gjs "nodes")
        msg-source (aget msg "source")
        msg-target (aget msg "target")
        source (aget nodes msg-source)
        target (aget nodes msg-target)]
                                        ;(.log js/console msg msg-target target nodes)
    (.moveTo ctx (aget source "x") (aget source "y"))
    (.lineTo ctx (aget target "x") (aget target "y"))))

(defn scale [p, s]
  #{"x" (* (aget p "x") s)
    "y" (* (aget p "y") s)})

(defn move-point [p, to, pct]
  (let [px (aget p "x")
        py (aget p "y")
        tox (aget to "x")
        toy (aget to "y")
        dx (- tox px)
        dy (- toy py)]
    (scale p 1)))

(defn draw-message
  [msg]
  (let [nodes (aget Gjs "nodes")
        source (aget nodes (aget msg "from"))
        target (aget nodes (aget msg "to"))
        src-x (aget source "x")
        src-y (aget source "y")
        tgt-x (aget target "x")
        tgt-y (aget target "y")
        slope (/ (- tgt-y src-y) (- tgt-x src-x))
        dff (js/Math.sqrt (+ (js/Math.pow (- src-x tgt-x) 2) (js/Math.pow (- src-y tgt-y) 2)))
        dist (if (= dff 0) node-radius dff)
        o ((js/d3.interpolate target source) (/ (- dist node-radius) dist) )
        ox (aget o "x")
        oy (aget o "y")
        p (aget msg "p")
        v ((js/d3.interpolate o target) (if (nil? p) 0 p)) 
        vx (aget v "x")
        vy (aget v "y")
        ]
    (.moveTo ctx ox oy)
    (.lineTo ctx vx vy)
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
    (aset ctx "strokeStyle" message-stroke-style)
    (.beginPath ctx)
    (.forEach current-edges draw-message)
    (.stroke ctx)


    (aset ctx "globalAlpha" old-message-alpha)
    (aset ctx "strokeStyle" old-message-stroke-style)
    (.beginPath ctx)
    (.forEach drawn-edges draw-old-message)
    (.stroke ctx)

    (aset ctx "globalAlpha" 0.1)
    (.forEach nodes draw-node)
    (aset ctx "fillStyle" "#000")
    (.fill ctx)
    (.translate ctx (- hw) (- hh))
    ))

(defn move-back [node i]
  (let [wd @window-dimensions
        w (aget wd "width")
        h (aget wd "height")
        hw (/ w 2)
        hh (/ h 2)
        x (+ (.-x node) hw)
        y (+ (.-y node) hh)
        dx (- (/ x w) .5)
        dy (- (/ y h) .5)
        ]
    (if (< (rand) (.pow js/Math (/ dx 0.5), 40)) (aset node "vx" (* dx -3)) )
    (if (< (rand) (.pow js/Math (/ dy 0.5), 40)) (aset node "vy" (* dy -3)) )
    ))

(defn limit-to-screen []
  (let [nodes (aget Gjs "nodes")]
    (.forEach nodes move-back)))

(def simulation
  (.. js/d3 forceSimulation
      ;(nodes (aget Gjs "nodes"))
      (force "link" (.. js/d3 forceLink (distance 50) (strength 0.04)))
      (force "collide" (.forceCollide js/d3 5))
      (force "charge" (.. js/d3 forceManyBody (distanceMin 40) (strength -1)))
      (force "boxed" limit-to-screen)
      ))

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
                     ; (.log js/console elapsed t)
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
                         (if (> (aget current-edges "length") 0)
                           (update-vis)
                           (if update-last-edges (do
                                                    (set! update-last-edges false)
                                                    (ticked))))
                       ))))
        timer (js/d3.timer timer-fn)
        ]
    (reset! timer-ref timer)
    ))


; for development
(defn on-js-reload [] (reagent/force-update-all))

(defn ui-control
  [state]
  [:div {:class "Control__ui"}
  [:button {:on-click #(reset! state state)} "restart"]])

(defn d3-canvas
  [state]
  (let [dom-node (reagent/atom nil)
        nodes (aget Gjs "nodes")]
    (reagent/create-class
     {
      :component-did-mount
      (fn [ this ]
        (context-update)
        (js/setTimeout update-vis delay-after-start)
        ;; (.. simulation (nodes (aget Gjs "nodes")) (on "tick" ticked)) 
        (.. simulation (nodes nodes) (on "tick" ticked))
        (.. simulation (force "link") (links force-edges))
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






