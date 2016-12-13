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
            [cljsjs.jsnetworkx]
            ))

(enable-console-print!)

;; define your app data so that it doesn't get over-written on reload

(defonce empty-state (atom {:nodes [] :edges []}))
(defonce state empty-state)
(def display (.getElementById js/document "display"))
;; (def context (.getContext display "2d"))
(def damping 0.999)
(def max-particles 250)
(def line-width 2)
(def window-dimensions (reagent/atom nil))



;; funcs

(def pi Math/PI)
(def two-pi (* 2 pi))

;; event handling

(defn on-window-resize [ evt ]
  (reset! window-dimensions [(.-innerWidth js/window) (.-innerHeight js/window)]))

;; do stuff

(defn context []
  (let [canvas (js/document.querySelector "canvas")]
    (set! (.-width canvas) (.-innerWidth js/window))
    (set! (.-height canvas) (.-innerHeight js/window))
    (.getContext canvas "2d")))

(defn drawing-board [])

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
;;        #(println "component did mount")
;;        :display-name "control-component"
;;        :should-component--update (fn [nP, nS] (false))
;;        :reagent-render 
;;        (fn [x y z ] [:div (str x " " y)])})))

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

