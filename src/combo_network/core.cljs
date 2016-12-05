(ns combo-network.core
  (:require-macros [reagent.ratom :refer [reaction]])
  (:require [reagent.core :as reagent :refer [atom]]
            [re-frame.core :refer [register-handler
                                   path
                                   register-sub
                                   dispatch
                                   dispatch-sync
                                   subscribe]]
            [cljsjs.d3]))

(enable-console-print!)

;; define your app data so that it doesn't get over-written on reload

(defonce app-state (atom {:nodes [] :edges []}))
(def display (.getElementById js/document "display"))
;; (def context (.getContext display "2d"))
(def damping 0.999)
(def max-particles 250)
(def line-width 2)

;; funcs

(def pi Math/PI)
(def two-pi (* 2 pi))

;; do stuff

(defn context []
  (let [canvas (js/document.querySelector "canvas")]
    (set! (.-width canvas) (.-innerWidth js/window))
    (set! (.-height canvas) (.-innerHeight js/window))
    (.getContext canvas "2d")))


(defn drawing-board []
  [:div {:id "content"}
   [:canvas {:id "canvas"}]])


(defn draw
  [ctx {:keys [x y old-x old-y color]}]
  (do
    (set! (.-strokeStyle ctx) color)
    (set! (.-lineWidth ctx) line-width)
    (.beginPath ctx)
    (.moveTo ctx old-x old-y)
    (.lineTo ctx x y)
    (.stroke ctx)))

(defn render
  []
  (.requestAnimationFrame js/window render)
  (let [{:keys [width height particles mouse]} @app-state]
    (.clearRect context 0 0 width height)
    (dotimes [n max-particles]
      (let [particle (nth particles n)
            x (:x mouse)
            y (:y mouse)
            p particle]
        (draw context p)
        (swap! app-state assoc-in [:particles n] p)))))


(defn animate []
  (js/setInterval (render) (/ 1000 60)))

(def attach-renderer
  (with-meta drawing-board
    {:component-did-mount animate}))

(defn app []
  [drawing-board]
  [attach-renderer])

(reagent/render-component [app]
                          (. js/document (getElementById "app")))
;; (defn ^:export run []
;;   (reagent/render-component [app]
;;                             (.-body js/document)))



(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
