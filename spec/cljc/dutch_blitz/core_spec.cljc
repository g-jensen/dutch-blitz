(ns dutch-blitz.core-spec
  (:require [c3kit.apron.corec :as ccc]
            [speclj.core #?(:clj :refer :cljs :refer-macros) [focus-context around context describe it should= should-be-nil should-contain should should-not before with-stubs]]
            [dutch-blitz.core :as sut]))

(defn- bad-shuffle [offset-atom coll]
  (let [shuffled (concat (drop @offset-atom coll)
                         (take @offset-atom coll))]
    (swap! offset-atom inc)
    shuffled))

(defn- ->three-pile-player [deck]
  {:post-piles [[(nth deck 0)]
                [(nth deck 1)]
                [(nth deck 2)]]
   :blitz-pile (take 10 (drop 3 deck))
   :wood-pile  []
   :hand       (drop 13 deck)})

(defn- has-hand? [state player]
  (seq (get-in state [:players player :hand])))

(defn- add-to-wood-pile-states [state player]
  (iterate #(sut/add-to-wood-pile % player) state))

(defn- add-to-wood-pile-until-empty-hand [state player]
  (let [states (add-to-wood-pile-states state player)]
    (first (drop-while #(has-hand? % player) states))))

(defn- add-to-wood-pile-until-empty-hand-states [state player]
  (let [states (add-to-wood-pile-states state player)]
    (take-while #(has-hand? % player) states)))

(describe "Dutch Blitz"

  (it "has a deck"
    (let [deck (for [n (range 0 10)
                     type (range 0 4)]
                 {:number n :type type})]
      (should= (map #(assoc % :player 0) deck)
               (sut/->deck 0))
      (should= (map #(assoc % :player 1) deck)
               (sut/->deck 1))))

  ; TODO - decouple these specs from implementation details
  (context "generating initial state"
    (it "for 2 players"
      (let [player-count 2
            shuffle-fn (partial bad-shuffle (atom 1))
            new-shuffle-fn (partial bad-shuffle (atom 1))
            deck-0 (shuffle-fn (sut/->deck 0))
            deck-1 (shuffle-fn (sut/->deck 1))]
        (should= {:dutch-piles []
                  :players     [{:post-piles [[(nth deck-0 0)]
                                              [(nth deck-0 1)]
                                              [(nth deck-0 2)]
                                              [(nth deck-0 3)]
                                              [(nth deck-0 4)]]
                                 :blitz-pile (take 10 (drop 5 deck-0))
                                 :wood-pile  []
                                 :hand       (drop 15 deck-0)}
                                {:post-piles [[(nth deck-1 0)]
                                              [(nth deck-1 1)]
                                              [(nth deck-1 2)]
                                              [(nth deck-1 3)]
                                              [(nth deck-1 4)]]
                                 :blitz-pile (take 10 (drop 5 deck-1))
                                 :wood-pile  []
                                 :hand       (drop 15 deck-1)}]}
                 (sut/init player-count new-shuffle-fn))))

    (it "for 3 players"
      (let [player-count 3
            shuffle-fn (partial bad-shuffle (atom 1))
            new-shuffle-fn (partial bad-shuffle (atom 1))
            deck-0 (shuffle-fn (sut/->deck 0))
            deck-1 (shuffle-fn (sut/->deck 1))
            deck-2 (shuffle-fn (sut/->deck 2))]
        (should= {:dutch-piles []
                  :players     [(->three-pile-player deck-0)
                                (->three-pile-player deck-1)
                                (->three-pile-player deck-2)]}
                 (sut/init player-count new-shuffle-fn))))

    (it "for 4 players"
      (let [player-count 4
            shuffle-fn (partial bad-shuffle (atom 1))
            new-shuffle-fn (partial bad-shuffle (atom 1))
            deck-0 (shuffle-fn (sut/->deck 0))
            deck-1 (shuffle-fn (sut/->deck 1))
            deck-2 (shuffle-fn (sut/->deck 2))
            deck-3 (shuffle-fn (sut/->deck 3))]
        (should= {:dutch-piles []
                  :players     [(->three-pile-player deck-0)
                                (->three-pile-player deck-1)
                                (->three-pile-player deck-2)
                                (->three-pile-player deck-3)]}
                 (sut/init player-count new-shuffle-fn))))

    (it "is invalid with more than 4 players"
      (should (sut/invalid-state? (sut/init 5 identity)))
      (should (sut/invalid-state? (sut/init 6 identity))))

    (it "is invalid with less than 2 players"
      (should (sut/invalid-state? (sut/init 1 identity)))
      (should (sut/invalid-state? (sut/init 0 identity))))

    (it "is not invalid with 2-4 players"
      (should (not-any? sut/invalid-state? (map #(sut/init % identity) [2 3 4]))))
    )

  (context "adding to wood-pile"
    (it "can be added to initially"
      (let [[player other-player :as players] [0 1]
            state (sut/init (count players) identity)]
        (should (every? true? (map #(sut/can-add-to-wood-pile? state %) players)))
        (should-not (sut/invalid-state? (sut/add-to-wood-pile state player)))
        (should-not (sut/invalid-state? (sut/add-to-wood-pile state other-player)))))

    (it "can be added to while hand is not empty"
      (let [[player other-player :as players] [0 1]
            state (sut/init (count players) identity)
            hand-states (add-to-wood-pile-until-empty-hand-states state player)]
        (should (every? #(sut/can-add-to-wood-pile? % player) hand-states))
        (should (every? #(sut/can-add-to-wood-pile? % other-player) hand-states))
        (should (not-any? #(sut/invalid-state? %) hand-states))))

    (it "cannot be added to if hand is empty"
      (let [[player other-player :as players] [0 1]
            state (sut/init (count players) identity)
            empty-hand-state (add-to-wood-pile-until-empty-hand state player)]
        (should-not (sut/can-add-to-wood-pile? empty-hand-state player))
        (should (sut/can-add-to-wood-pile? empty-hand-state other-player))
        (should (sut/invalid-state? (sut/add-to-wood-pile empty-hand-state player)))
        (should-not (sut/invalid-state? (sut/add-to-wood-pile empty-hand-state other-player)))))

    (it "adds to an empty wood-pile"
      (let [player-count 2
            player 0
            state (sut/init player-count identity)
            hand (get-in state [:players player :hand])]
        (should= (-> state
                     (assoc-in [:players player :wood-pile]
                               (reverse (take 3 hand)))
                     (assoc-in [:players player :hand]
                               (drop 3 hand)))
                 (sut/add-to-wood-pile state player))))

    (it "adds to a non-empty wood-pile"
      (let [player-count 2
            player 0
            state (-> (sut/init player-count identity)
                      (sut/add-to-wood-pile player))
            wood-pile (get-in state [:players player :wood-pile])
            hand (get-in state [:players player :hand])]
        (should= (-> state
                     (assoc-in [:players player :wood-pile]
                               (concat (reverse (take 3 hand))
                                       wood-pile))
                     (assoc-in [:players player :hand]
                               (drop 3 hand)))
                 (sut/add-to-wood-pile state player))))

    (it "adds to specified player's pile"
      (let [player-count 2
            player 1
            state (-> (sut/init player-count identity)
                      (sut/add-to-wood-pile player))
            wood-pile (get-in state [:players player :wood-pile])
            hand (get-in state [:players player :hand])]
        (should= (-> state
                     (assoc-in [:players player :wood-pile]
                               (concat (reverse (take 3 hand))
                                       wood-pile))
                     (assoc-in [:players player :hand]
                               (drop 3 hand)))
                 (sut/add-to-wood-pile state player))))
    )

  (context "resetting wood-pile"
    (it "is not cyclable initially"
      (let [[player other-player :as players] [0 1]
            state (sut/init (count players) identity)]
        (should-not (sut/can-reset-wood-pile? state player))
        (should-not (sut/can-reset-wood-pile? state other-player))))

    (it "is resettable if hand is empty"
      (let [[player other-player :as players] [0 1]
            state (sut/init (count players) identity)
            empty-hand-state (add-to-wood-pile-until-empty-hand state player)]
        (should (sut/can-reset-wood-pile? empty-hand-state player))
        (should-not (sut/can-reset-wood-pile? empty-hand-state other-player))))

    (it "is not cyclable before hand is empty"
      (let [[player other-player :as players] [0 1]
            state (sut/init (count players) identity)
            hand-states (add-to-wood-pile-until-empty-hand-states state player)]
        (should (every? not (map #(sut/can-reset-wood-pile? % player) hand-states)))
        (should (every? not (map #(sut/can-reset-wood-pile? % other-player) hand-states))))))

  (context "cycling hand"
    (it "is not cyclable initially"
      (let [[player other-player :as players] [0 1]
            state (sut/init (count players) identity)]
        (should-not (sut/can-cycle-hand? state player))
        (should-not (sut/can-cycle-hand? state other-player))))

    (it "is cyclable after going through your entire hand and not playing"
      (let [[player other-player :as players] [0 1]
            state (sut/init (count players) identity)
            empty-hand-state (add-to-wood-pile-until-empty-hand state player)]
        (should (sut/can-cycle-hand? empty-hand-state player))
        (should-not (sut/can-cycle-hand? empty-hand-state other-player))))

    (it "is not cyclable before hand is empty"
      (let [[player other-player :as players] [0 1]
            state (sut/init (count players) identity)
            hand-states (add-to-wood-pile-until-empty-hand-states state player)]
        (should (every? not (map #(sut/can-cycle-hand? % player) hand-states)))
        (should (every? not (map #(sut/can-cycle-hand? % other-player) hand-states)))))
    )

  ; add to wood pile (done - may need to add rule for when 1 or 2 cards are left in hand)
  ; reset hand
  ; cycle hand (WIP)
  ; move card to dutch pile
  ; move card to post pile

  )