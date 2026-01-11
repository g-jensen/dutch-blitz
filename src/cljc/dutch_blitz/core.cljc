(ns dutch-blitz.core)

(def number-count 10)
(def type-count 4)

(defn ->deck [player]
  (for [n (range 0 number-count)
        type (range 0 type-count)]
    {:player player :number n :type type}))

(defn- piles [n deck]
  (map #(vec [(nth deck %)]) (range 0 n)))

(def two-player-post-pile-count 5)
(def normal-post-pile-count 3)

(defn- post-piles [player-count deck]
  (if (= player-count 2)
    (piles two-player-post-pile-count deck)
    (piles normal-post-pile-count deck)))

(defn- post-piles-rest [player-count deck]
  (if (= player-count 2)
    (drop two-player-post-pile-count deck)
    (drop normal-post-pile-count deck)))

(defn- build-post-piles [player-count deck]
  [(post-piles player-count deck)
   (post-piles-rest player-count deck)])

(def initial-blitz-pile-size 10)

(defn- blitz-pile [deck]
  (take initial-blitz-pile-size deck))

(defn- blitz-pile-rest [deck]
  (drop initial-blitz-pile-size deck))

(defn- build-blitz-pile [deck]
  [(blitz-pile deck)
   (blitz-pile-rest deck)])

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

(defn can-add-to-wood-pile? [state player]
  (boolean (seq (hand state player))))

(defn- put-3-in-wood-pile [state player]
  (let [hand (hand state player)]
    (update-in
      state
      [:players player :wood-pile]
      #(concat (reverse (take 3 hand)) %))))

(defn- take-3-from-hand [state player]
  (update-in state (hand-path player) #(drop 3 %)))

(defn- player-state [state player]
  (get (:players state) player))

(defn- maybe-set-cyclable [state player]
  (if (empty? (hand state player))
    (assoc-in state [:players player :cyclable?] true)
    state))

(defn- maybe-cannot-add-to-wood-pile [state player]
  (when-not (can-add-to-wood-pile? state player)
    invalid-state))

(defn add-to-wood-pile [state player]
  (or (maybe-cannot-add-to-wood-pile state player)
      (-> state
          (put-3-in-wood-pile player)
          (take-3-from-hand player)
          (maybe-set-cyclable player))))

(defn can-cycle-hand? [state player]
  (:cyclable? (player-state state player)))