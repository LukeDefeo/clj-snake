(ns snake.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [clojure.core.match :as core]))

;definitions
;note snake grid respresented as a kind of x/y cordinate system where each cell is either empty, contains
;part of the snake or the apple. The grid has bounds defined below. The origin is top left and down
;is positive in the y axis similar to mobile gui toolkits
(def grid-width 30)
(def grid-height 16)
(def grid-scale-factor 10)
(def screen-width (* grid-width grid-scale-factor))
(def screen-height (* grid-height grid-scale-factor))

;pure snake functions

(defn generate-apple []
  {:type     :apple,                                        ;to distinguish this map from snake map
   :location [(rand-int grid-width) (rand-int grid-height)]}) ;x/y cords or apple to render

(defn generate-snake []

  {:type      :snake,                                       ;to distinguish this map from apple map
   :body      '([3 0] [2 0] [1 0] [0 0])                    ;the snakes position to render starting from head going to the tail
   :direction [1 0]})                                       ;represents direction snake should move on next tick, first
                                                            ;first column is x direction, second is y, negative y means

(defn generate-game-state []
  {:snake (generate-snake)
   :apple (generate-apple)
   :alive true})

(defn turn-snake [snake new-direction]
  "changes the direction of the snake"
  (assoc snake :direction new-direction))

(declare new-head)

(defn move
  "Moves the snakes position in the direction in the snake map. If there is food in that slot the snake will head
  will now occupy that slot and the snake wont really move. It is is a free space then the tail will be lost to give the illusion
   the snake is moving"
  [{:keys [body direction] :as snake} is-eating]

  (assoc snake :body
               (cons
                 (new-head (first body) direction)
                 (if is-eating
                   body
                   (butlast body)))))


(defn new-head
  "Find new head cordinate base on old head cordinate and current direction"
  [[old-head-x old-head-y] [direction-x direction-y]]
  [(+ old-head-x direction-x) (+ old-head-y direction-y)])



(defn snake-head-intersects-body
  "check if the snake has hit itself, expects whole snake body as arg"
  [[head & body]]
  (contains? (set body) head))

(snake-head-intersects-body (:body (generate-snake)))

(defn head-out-of-bounds
  "checks if snake is out of bounds expect the head of the snake cords as the arg"
  [[head-x head-y]]
  (or
    (< head-x 0)
    (< head-y 0)
    (> head-x grid-width)
    (> head-y grid-height)))


(defn opposite-directions? [[a-x a-y] [b-x b-y]]
  "checks if two directions would are opposite and would cancel each other out
  works by adding up the two vectors and checking they equal 0"
  (and
    (= (+ a-x b-x) 0)
    (= (+ a-y b-y) 0)))

;test cases
(= true (opposite-directions? [1 0] [-1 0]))
(= true (opposite-directions? [-1 1] [1 0]))
(= true (opposite-directions? [0 1] [0 -1]))
(= false (opposite-directions? [0 1] [1 0]))


(defn game-over
  "check for game over conditions"
  [body]
  (or
    (snake-head-intersects-body body)
    (head-out-of-bounds (first body))))


;quil specific pure functions

(defn update-state [state]
  "Moves the snake one unit in the grid in the last pressed direction.
  Check for game end and if so marks"
  ;(println "game-state " state)
  (if (:alive state)
    (let [moved-state (assoc state :snake (move (:snake state) false))]
      (assoc moved-state :alive (not (game-over (get-in moved-state [:snake :body]))))) ;need this nested let otherwise snake goes an extra step
    state))


(def state (generate-game-state))
state

(def sample-map {:a 1 :b 2})

(assoc sample-map :a 2 :b 5 :c 24)

(move (:snake state) false)
(update-state state)

(defn key-to-cord-direction [key]
  "converts from quil framework key event to a direction in the snake world"
  (cond (= key :left) [-1 0]
        (= key :right) [1 0]
        (= key :up) [0 -1]
        (= key :down) [0 1]))

(defn move-snake [key-pressed state]
  (let [requested-cord-direction (key-to-cord-direction key-pressed)
        current-cord-direction (get-in state [:snake :direction])]

    (if (opposite-directions? current-cord-direction requested-cord-direction) ;dont turn the snake if the new key is the opposite direction
      state
      (assoc state :snake (turn-snake (:snake state) requested-cord-direction)))))


(defn key-pressed-handler
  [state {key-pressed :key}]
  "Updates the snakes direction flag with the new key press"
  (core/match [(:alive state) key-pressed]
              [false :n] (generate-game-state)
              [true (:or :up :down :right :left)] (move-snake key-pressed state)
              :else state))



(defn cord-to-rect
  "converts cordinates on the snake grid to a rect to render"
  [[x y]]
  (list (* x grid-scale-factor) (* y grid-scale-factor) grid-scale-factor grid-scale-factor))

;quil impure
;
;(take-while #(< % 4) [1 2 3 4 3 2 1])
;(drop-while #(< % 4) [1 2 3 4 3 2 1])
;(filter #(< % 4) [1 2 3 4 3 2 1])
;(into [] (remove #(< % 4) [1 2 3 4 3 2 1]))
;
;(def lazy-filtered (filter #(< % 4) [1 2 3 4 3 2 1]))
;
;(defn the-truth [] true)
;(def false-hood (complement the-truth))
;(false-hood)
;
;(realized? lazy-filtered)
;
;(into)


(defn setup []
  "setup basic things and returns the initial game state"
  (q/frame-rate 10)
  (q/background 0)
  (generate-game-state))


(defn draw-snake-body
  "draws the snake at its current position"
  [body]
  ; Clear the previous snake by filling it with black color.
  (q/background 0)
  ;setup fill for the snake
  (q/fill 100 100 100)
  (doseq [snake-cord body]
    (apply q/rect (cord-to-rect snake-cord))))

(defn display-game-over
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
    (draw-snake-body (get-in state [:snake :body]))
    (display-game-over)))




(q/defsketch snake
             :title "Snake 1, walls kill the snake"
             :size [screen-width screen-height]
             :setup setup                                   ; setup function called only once, during sketch initialization.
             :key-pressed key-pressed-handler
             :update update-state                           ; update-state is called on each iteration before draw-state.
             :draw draw-state
             :features [:keep-on-top]
             :middleware [m/fun-mode])
