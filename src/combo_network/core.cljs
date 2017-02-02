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
(def G (ggen/erdos-renyi 10 0.2))

(defonce empty-state (atom {:nodes [] :edges []}))
(defonce state empty-state)
(def display (.getElementById js/document "display"))
;; (def context (.getContext display "2d"))
;; (def damping 0.999)
;; (def max-particles 250)
(def line-width 2)
(def ctx (reagent/atom nil))
(def window-dimensions (reagent/atom nil))


(def pi Math/PI)
(def two-pi (* 2 pi))

(defn context []
  (let [canvas (js/document.querySelector "canvas")]
    (set! (.-width canvas) (.-innerWidth js/window))
    (set! (.-height canvas) (.-innerHeight js/window))
    (.log js/console canvas)
    (.log js/console (.getContext canvas "2d"))
    (.getContext canvas "2d")))

(defn drawing-board [])

(defn context-update [ ]
  (reset! ctx (context)))

(defn on-window-resize []
  (reset! window-dimensions {:width (.-innerWidth js/window) :height (.-innerHeight js/window)}))

(defn ticked
  []
  ; (.log js/console @ctx)
  (if @ctx
    (
     (.clearRect ctx (:width window-dimensions) (:height window-dimensions))
     (.strokeStyle ctx "#123456"))))

(def simulation
  (.. js/d3 forceSimulation
      (force "center" (.forceCenter js/d3 (/ (:width window-dimensions) 2) (/ (:height window-dimensions) 2)))
      (force "link" (.. js/d3 (forceLink) (distance 50) (strength 0.02)))
      (force "link" (.. js/d3 forceLink (distance 50) (strength 0.02)))
      (force "collide" (.forceCollide js/d3 5))
      (force "charge" (.. js/d3 forceManyBody (distanceMin 20) (strength -1)))
      ))
(.. simulation (nodes (:nodes G)) (on "tick" ticked))

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
        (.log js/console (reagent/dom-node this))
        (context-update )
        (reset! dom-node (reagent/dom-node this)))

      :reagent-render
      (fn [ ]
        @window-dimensions ;; Trigger re-render on window resizes
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
