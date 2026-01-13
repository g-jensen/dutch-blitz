(ns dutch-blitz.core)

(def number-count 10)
(def type-count 4)

(defn ->deck [player]
  (for [n (range 0 number-count)
        type (range 0 type-count)]
    {:player player :number n :type type}))

(def two-player-post-pile-count 5)
(def normal-post-pile-count 3)

(defn- post-pile-count [player-count]
  (if (= player-count 2)
    two-player-post-pile-count
    normal-post-pile-count))

(defn- initial-post-piles [post-pile-count deck]
  (map #(vec [(nth deck %)]) (range 0 post-pile-count)))

(defn- initial-post-piles-rest [post-pile-count deck]
  (drop post-pile-count deck))

(defn- build-post-piles [player-count deck]
  (let [post-pile-count (post-pile-count player-count)]
    [(initial-post-piles post-pile-count deck)
     (initial-post-piles-rest post-pile-count deck)]))

(def initial-blitz-pile-size 10)

(defn- initial-blitz-pile [deck]
  (take initial-blitz-pile-size deck))

(defn- initial-blitz-pile-rest [deck]
  (drop initial-blitz-pile-size deck))

(defn- build-blitz-pile [deck]
  [(initial-blitz-pile deck)
   (initial-blitz-pile-rest deck)])

(defn- ->player [player-count shuffle-fn player]
  (let [hand (shuffle-fn (->deck player))
        [post-piles hand] (build-post-piles player-count hand)
        [blitz-pile hand] (build-blitz-pile hand)]
    {:post-piles post-piles
     :blitz-pile blitz-pile
     :wood-pile  []
     :hand       hand}))

(def ^:private invalid-state :invalid-state)

(defn invalid-state? [state]
  (= state invalid-state))

(defn- maybe-invalid-player-count [player-count]
  (when (or (< player-count 2) (> player-count 4))
    invalid-state))

(defn init [player-count shuffle-fn]
  (or (maybe-invalid-player-count player-count)
      {:dutch-piles []
       :players (mapv #(->player player-count shuffle-fn %) (range 0 player-count))}))

(defn- hand-path [player]
  [:players player :hand])

(defn- hand [state player]
  (get-in state (hand-path player)))

(defn- can-add-to-wood-pile? [state player]
  (boolean (seq (hand state player))))

(defn- wood-pile-path [player]
  [:players player :wood-pile])

(defn- put-3-in-wood-pile [state player]
  (let [hand (hand state player)]
    (update-in
      state
      (wood-pile-path player)
      #(concat (reverse (take 3 hand)) %))))

(defn- take-3-from-hand [state player]
  (update-in state (hand-path player) #(drop 3 %)))

(defn- player-state [state player]
  (get (:players state) player))

(defn- maybe-cannot-add-to-wood-pile [state player]
  (when-not (can-add-to-wood-pile? state player)
    invalid-state))

(defn add-to-wood-pile [state player]
  (or (maybe-cannot-add-to-wood-pile state player)
      (-> state
          (put-3-in-wood-pile player)
          (take-3-from-hand player))))

(defn- can-reset-wood-pile? [state player]
  (empty? (hand state player)))

(defn- wood-pile [state player]
  (get-in state (wood-pile-path player)))

(defn- replenish-hand [state player]
  (let [wood-pile (wood-pile state player)]
    (assoc-in state (hand-path player) (reverse wood-pile))))

(defn- clear-wood-pile [state player]
  (assoc-in state (wood-pile-path player) []))

(defn- maybe-cannot-reset-wood-pile [state player]
  (when-not (can-reset-wood-pile? state player)
    invalid-state))

(defn- mark-cyclable [state player]
   (assoc-in state [:players player :cyclable?] true))

(defn reset-wood-pile [state player]
  (or (maybe-cannot-reset-wood-pile state player)
      (-> state
          (replenish-hand player)
          (clear-wood-pile player)
          (mark-cyclable player))))

(defn can-cycle-hand? [state player]
  (:cyclable? (player-state state player)))