(ns snake.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [clojure.core.match :as core]))

;TODO things to do
;1. add apples and growing of the snake
;2. add pause feature
;3. snake 2 rules
;4. add abilty to change speed as part of difficulty
;5. publish to github


;definitions
;note snake grid respresented as a kind of x/y cordinate system where each cell is either empty, contains
;part of the snake or the apple. The grid has bounds defined below. The origin is top left and down
;is positive in the y axis similar to mobile gui toolkits
(def grid-width 30)
(def grid-height 16)
(def grid-scale-factor 10)
(def screen-width (* grid-width grid-scale-factor))
(def screen-height (* grid-height grid-scale-factor))

;generating state

(defn generate-snake []
  "All Cordinates in a vectors of two elements, first item is x, second is y. Origin is top left so +ive y is down
  :body - the snakes position to render starting from head going to the tail
  :direction - represents direction snake should move on next tick, first"
  {
   :body      '([3 0] [2 0] [1 0] [0 0])
   :direction [1 0]})

(defn generate-apple
  ;TODO make apple not generate where the snake already is
  "generate random point for apple"
  []
  [(rand-int grid-width) (rand-int grid-height)])

(defn initial-game-state []
  {:snake (generate-snake)
   :apple (generate-apple)
   :alive true })

;mostly for readability


;snake movement

(def get-head first)

(defn turn-snake [snake new-direction]
  "changes the direction of the snake"
  (assoc snake :direction new-direction))

(defn new-head
  "Find new head cordinate base on old head cordinate and current direction"
  [[old-head-x old-head-y] [direction-x direction-y]]
  [(+ old-head-x direction-x) (+ old-head-y direction-y)])


;apple

(defn snake-eaten-apple [head apple]
  (= head apple))

;game over checking

(defn snake-head-intersects-body
  "check if the snake has hit itself, expects whole snake body as arg"
  [[head & body]]
  (contains? (set body) head))

(defn head-out-of-bounds
  "checks if snake is out of bounds expect the head of the snake cords as the arg"
  [[head-x head-y]]
  (or
    (< head-x 0)
    (< head-y 0)
    (> head-x grid-width)
    (> head-y grid-height)))

(defn game-over
  "check for game over conditions"
  [body]
  (or
    (snake-head-intersects-body body)
    (head-out-of-bounds (first body))))

(def alive (complement game-over))

;misc

(defn opposite-directions? [[a-x a-y] [b-x b-y]]
  "checks if two directions would are opposite.
  Works by adding up the two vectors and checking they equal 0 -
  this implies the vectors cancel out and the snakes vector velocity is [0 0] which is not allowed"
  (and
    (= (+ a-x b-x) 0)
    (= (+ a-y b-y) 0)))

;quil specific pure functions - update cycle

; update process
; First check check if the game is running or player dead, if dead do nothing
; calculate the new position of the of the snake
; cond
;     if the head hits a wall, mark the game as ended ,remove the last, update the body with the new
;     if its the head hits an apple, generate a new apple and, update the body of the snake
;     if its a free space, remove the last  just update the body with the new
;

(defn update-alive [{{body :body} :snake  :as state}]
  (assoc state :alive (alive body)))

(defn grow-snake-body
  "Finds the new position the head is moving towrads and adds that to the snakes body, returns the new body"
  [{:keys [body direction] :as snake}]
  (let [cur-head (get-head body)
        new-head (new-head cur-head direction)]
    (cons new-head body)))

(defn update-snake
  ;TODO  should be called grow snake but already have that, this just does structuring
  [{snake :snake :as state}]
  (let [new-body (grow-snake-body snake)]
    (assoc-in state [:snake :body] new-body)))

(defn update-apple [{apple :apple {body :body} :snake :as state}]
  "check if snake has eaten the apple, if so generate a new apple, if not cut off the last element to imply movement
  Maybe could have a better name since it also updates the snake if the snake didnt eat the apple"
  (if (snake-eaten-apple (get-head body) apple)
    (assoc state :apple (generate-apple))
    (update-in state [:snake :body] butlast)))

(defn update-state [state]
  "Moves the snake one unit in the grid in the last pressed direction.
  Check for game end and if so marks"
  ;(println "game-state " state)
  (if (:alive state)
    (-> state
        update-snake                                        ;move the snake 1 tick
        update-apple                                        ;update the apple or cut the snakes tail
        update-alive                                        ;update alive flag
        )
    state))

;quil specific pure functions - event handling cycle

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

    (if (opposite-directions? current-cord-direction requested-cord-direction) ;dont turn the snake if the new key is the opposite direction
      state
      (assoc state :snake (turn-snake (:snake state) requested-cord-direction)))))

(defn key-pressed-handler
  [state {key-pressed :key}]
  ""
  (core/match [(:alive state) key-pressed]
              [false :n] (initial-game-state)
              [true (:or :up :down :right :left)] (update-state-snake-turn key-pressed state)
              :else state))

(defn cord-to-rect
  "converts cordinates on the snake grid to a rect to render"
  [[x y]]
  (list (* x grid-scale-factor) (* y grid-scale-factor) grid-scale-factor grid-scale-factor))

;quil impure

(defn setup []
  "setup basic things and returns the initial game state"
  (q/frame-rate 10)
  (q/background 0)
  (initial-game-state))

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

(q/defsketch snake
             :title "Snake 1, walls kill the snake"
             :size [screen-width screen-height]
             :setup setup                                   ; setup function called only once, during sketch initialization.
             :key-pressed key-pressed-handler
             :update update-state                           ; update-state is called on each iteration before draw-state.
             :draw draw-state
             :features [:keep-on-top]
             :middleware [m/fun-mode])
