(ns snake.main
  (:require
    [quil.core :as q]
    [snake.core :as snake]
    [snake.quil.events :as events]
    [snake.quil.update :as update]
    [snake.quil.draw :as draw]
    [quil.middleware :as m])
  (:gen-class))

(defn setup []
  "setup basic things and returns the initial game state"
  (q/frame-rate 10)
  (q/background 0)
  (snake/initial-game-state))

(defn -main [& args]
  (q/defsketch snake
               :title "Snake 1, walls kill the snake"
               :size [snake/screen-width snake/screen-height]
               :setup setup                           ; setup function called only once, during sketch initialization.
               :key-pressed events/key-pressed-handler
               :update update/game-tick                     ; update-state is called on each iteration before draw-state.
               :draw draw/draw-state
               :features [:keep-on-top]
               :middleware [m/fun-mode]))

(-main)