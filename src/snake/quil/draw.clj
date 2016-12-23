(ns snake.quil.draw
  (:require [quil.core :as q])
  (:use snake.core))

;util

(defn cord-to-rect
  "converts cordinates on the snake grid to a rect to render"
  [[x y]]
  (list (* x grid-scale-factor) (* y grid-scale-factor) grid-scale-factor grid-scale-factor))

;impure drawing functions

(defn draw-snake-body
  "draws the snake at its current position"
  [body]
  ;setup fill for the snake
  (q/fill 100 100 100)
  (doseq [snake-cord body]
    (apply q/rect (cord-to-rect snake-cord))))

(defn draw-apple [apple-location]
  ;setup fill for the apple
  (q/fill 20 200 80)
  (apply q/rect (cord-to-rect apple-location)))

(defn draw-game-playing [state]
  ; Clear the previous state by filling it with black color.
  (q/background 0)
  (draw-snake-body (get-in state [:snake :body]))
  (draw-apple (get state :apple)))

(defn draw-game-over
  "Shows a prompt saying game over"
  []
  ;setup fill for the text
  (q/fill 200 20 20)
  (q/text-size 16)
  (q/rect-mode :center)
  (q/text-align :center)
  (q/text "Game over press enter to retry" (/ screen-width 2) (/ screen-height 2) 100 100))


(defn draw-state [state]
  "draws the current state, main side affect point, the output is ignored"
  (if (:alive state)
    (draw-game-playing state)
    (draw-game-over)))