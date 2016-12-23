(ns snake.core)

;TODO things to do
;1. add apples and growing of the snake
;2. add pause feature
;3. snake 2 rules
;4. add abilty to change speed as part of difficulty
;5. publish to github
;6. q to quit

;BUGS
;1 if the snake is going to the right and you manae to press down then left before the
;next frame then the snake will go back on itself which is not allowed. Need a way to detect this


;definitions
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

;snake movement

(def get-head first)

(defn turn-snake [snake new-direction]
  "changes the direction of the snake"
  (assoc snake :direction new-direction))

(defn new-head
  "Find new head cordinate base on old head cordinate and current direction"
  [[old-head-x old-head-y] [direction-x direction-y]]
  [(+ old-head-x direction-x) (+ old-head-y direction-y)])

(defn grow-snake-body
  "Finds the new position the head is moving towrads and adds that to the snakes body, returns the new body"
  [{:keys [body direction] :as snake}]
  (let [cur-head (get-head body)
        new-head (new-head cur-head direction)]
    (cons new-head body)))

(defn opposite-directions? [[a-x a-y] [b-x b-y]]
  "checks if two directions would are opposite.
  Works by adding up the two vectors and checking they equal 0 -
  this implies the vectors cancel out and the snakes vector velocity is [0 0] which is not allowed"
  (and
    (= (+ a-x b-x) 0)
    (= (+ a-y b-y) 0)))

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
