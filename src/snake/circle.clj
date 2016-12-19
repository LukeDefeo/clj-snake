(ns snake.circle
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(def min-r 10)

(defn setup []
  ; initial state
  {:x 0 :y 0 :r min-r})

(defn update [state]
  ; increase radius of the circle by 1 on each frame
  (update-in state [:r] inc))

(defn draw [state]
  (q/background 255)
  (q/fill 12 15 199)
  (q/rect (:x state) (:y state) (:r state) (:r state)))

; decrease radius by 1 but keeping it not less than min-r
(defn shrink [r]
  (max min-r (dec r)))

(defn mouse-moved [state event]
  (-> state
      ; set circle position to mouse position
      (assoc :x (:x event) :y (:y event))
      ; decrease radius
      (update-in [:r] shrink)))

(defn key-pressed-handler [old-state event]
  (println event)
  (println old-state)
  old-state)


(q/exit)
(q/defsketch circle
             :size :fullscreen
             :setup setup
             :draw draw
             :update update
             :key-pressed key-pressed-handler
             :mouse-moved mouse-moved
             :middleware [m/fun-mode])
