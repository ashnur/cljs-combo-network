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

(defn app []
  [:div {:id "content"}
   [:canvas {:id "canvas"}]])

(reagent/render [app]
                (js/document.getElementById "app"))


(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
