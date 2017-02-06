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

(defn touches
  [node]
  (fn [link]
    (or (= (.-source link) node) (= (.-target link)))))

(def delay-after-start 2000)
(def G (ggen/erdos-renyi 100 0.2))

(defonce empty-state (atom G))
(defonce state empty-state)
(def init-node (rand-int (.-length (:nodes G))))
; graph.edges.filter(touches(initNode)).map(addDirections)
;; const addDirections = (link) => {
;;                                  const to = messages.indexOf(link.source) > -1 ? link.target : link.source
;;                                  const from = messages.indexOf(link.source) > -1 ? link.source : link.target
;;                                  return R.merge({to, from, p: 0}, link)
;;                                  }

(defn add-directions [link]
  (let [to ()
        from ()
        dflt {:to to :from from :p 0}]
    (merge dflt (js->clj link))))

(def current-edges (.. (.-edges G) (filter (touches init-node)) (map add-directions)))

(def display (.getElementById js/document "display"))
;; (def context (.getContext display "2d"))
;; (def damping 0.999)
;; (def max-particles 250)
(def line-width 2)
(def ctx nil)
(def message-alpha 0.22)
(def message-stroke-style "#123456")
(def window-dimensions {})


(def pi Math/PI)
(def two-pi (* 2 pi))

(defn context []
  (let [canvas (js/document.querySelector "canvas")]
    (set! (.-width canvas) (.-innerWidth js/window))
    (set! (.-height canvas) (.-innerHeight js/window))
    (.getContext canvas "2d")))

(defn drawing-board [])

(defn context-update [ ]
  (set! ctx (context)))

(defn on-window-resize []
  (set! window-dimensions {:width (.-innerWidth js/window) :height (.-innerHeight js/window)}))

(defn draw-node
  [d]
  ;; (.log js/console d)
  (.moveTo ctx (+ d.x 2.5) d.y)
  (.arc ctx d.x d.y 2.5 0 2 * (.-PI js/Math)))

(def drawn-edges [])
(defn draw-old-message [msg]
  (let [source (:source msg)
        target (:target msg)]
    (.log js/console source target msg)))

(defn draw-message
  [msg]
  (let [source ('.-(.-from msg) (:nodes G))
        target ('.-(.-to msg) (:nodes G))
        [x,y] [(.interpolate js/d3 source target) (:p msg)]]
        (.moveTo ctx (.-x source) (.-y source))
        (.lineTo ctx x y))
  )

(defn ticked
  []
  (.log js/console window-dimensions)
  (.clearRect ctx 0 0 (:width window-dimensions) (:height window-dimensions))

  (aset ctx "globalAlpha" message-alpha)
  (.beginPath ctx)
  (doseq [msg drawn-edges] (draw-old-message msg))
  (aset ctx "strokeStyle" message-stroke-style)
  (.stroke ctx)

  (.beginPath ctx)
  (doseq [msg current-edges] (draw-message msg))
  (.stroke ctx)

  (.beginPath ctx)
  (doseq [node (:nodes G)] (draw-node node))
  (aset ctx "globalAlpha" 0.3)
  (aset ctx "fillStyle" (.interpolateViridis js/d3 0.3))
  (.fill ctx)
  )

(def simulation
  (.. js/d3 forceSimulation
      (force "center" (.forceCenter js/d3 (/ (:width window-dimensions) 2) (/ (:height window-dimensions) 2)))
      (force "link" (.. js/d3 (forceLink) (distance 50) (strength 0.02)))
      (force "link" (.. js/d3 forceLink (distance 50) (strength 0.02)))
      (force "collide" (.forceCollide js/d3 5))
      (force "charge" (.. js/d3 forceManyBody (distanceMin 20) (strength -1)))
      ))

(defn ui-control
  [state]
  [:div {:class "Control__ui"}
  [:button {:on-click #(reset! state empty-state)} "restart"]])

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
        ;; (js/setTimeout update-vis delay-after-start)
        ;; (.log js/console (reagent/dom-node this))
        (context-update)
        (.. simulation (nodes (:nodes G)) (on "tick" ticked))
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

(defn on-js-reload [])
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)


;; (defn draw
;;   [ctx {:keys [x y old-x old-y color]}]
;;   (do
;;     (set! (.-strokeStyle ctx) color)
;;     (set! (.-lineWidth ctx) line-width)
;;     (.beginPath ctx)
;;     (.moveTo ctx old-x old-y)
;;     (.lineTo ctx x y)
;;     (.stroke ctx)))

;; (defn control-component 
;;   [x y z]
;;   (let [canvas null]
;;     (reagent/create-class
;;       {:component-did-mount
;;        #(swap! canvas (js/document.getElementBy))
;;        :component-will-mount
;;        :display-name "control-component"
;;        :should-component--update (fn [nP, nS] (false))
;;        :reagent-render 
;;        (fn [x y z ] [:div (str x " " y)])})))

;; (defn update-vis
;;   []
;;   (let [timer (.timer d3 (fn [elapsed]
;;                            (let [progress (/ elapsed travel-time)]
;;                              (if (< elapsed travel-time)
;;                                ())))]))
