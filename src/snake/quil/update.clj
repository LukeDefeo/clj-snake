(ns snake.quil.update
  (:use snake.core))

;quil specific - game tick

(defn update-alive [{{body :body} :snake  :as state}]
  (assoc state :alive (alive body)))

(defn update-state-apple [{apple :apple {body :body} :snake :as state}]
  "check if snake has eaten the apple, if so generate a new apple, if not cut off the last element to imply movement
  Maybe could have a better name since it also updates the snake if the snake didnt eat the apple"
  (if (snake-eaten-apple (get-head body) apple)
    (assoc state :apple (generate-apple))
    (update-in state [:snake :body] butlast)))

(defn update-state-snake-grow
  ;TODO is this overengineering, it mainly does destructuring and just calls a another method
  "Updats the state with a snake grown by one,"
  [{snake :snake :as state}]
  (let [new-body (grow-snake-body snake)]
    (assoc-in state [:snake :body] new-body)))

(defn update-state
  "Orchestrates changes of the game state for one tick for when the snake is alive"
  [state]
  (-> state
      update-state-snake-grow                                     ;move the snake 1 tick
      update-state-apple                                          ;update the apple or cut the snakes tail
      update-alive))                                              ;update alive flag

(defn game-tick [state]
  "Called by quil for every frame"
  (if (:alive state)
    (update-state state)
    state))

