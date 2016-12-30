(ns snake.quil.events
  (:require [snake.core :as snake]
            [clojure.core.match :as core]))

;quil specific - event handling cycle

(defn key-to-cord-direction [key]
  "converts from quil framework key event to a direction in the snake world"
  (cond (= key :left) [-1 0]
        (= key :right) [1 0]
        (= key :up) [0 -1]
        (= key :down) [0 1]))

(defn update-state-snake-turn
  ;TODO think of a better fn name.
  "Updates the snakes direction flag with the new key press, Checks if the direction player has last pressed
  is the opposite to current direction, cant go backwards so do nothing,"
  [key-pressed state]
  (let [requested-cord-direction (key-to-cord-direction key-pressed)
        current-cord-direction (get-in state [:snake :direction])]

    (if (snake/opposite-directions? current-cord-direction requested-cord-direction) ;dont turn the snake if the new key is the opposite direction
      state
      (assoc state :snake (snake/turn-snake (:snake state) requested-cord-direction)))))

(defn key-pressed-handler
  [state {key-pressed :key}]
  "Top level handler called by quil when a key is pressed"
  (core/match [(:alive state) key-pressed]
              [false :n] (snake/initial-game-state)
              [true (:or :up :down :right :left)] (update-state-snake-turn key-pressed state)
              :else state))
