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

(defn- ->card [number type player]
  {:number number :type type :player player})

(defn- with-blitz-pile [state player cards]
  (assoc-in state [:players player :blitz-pile] cards))

(defn- with-post-piles [state player piles]
  (assoc-in state [:players player :post-piles] piles))

(defn- with-dutch-piles [state piles]
  (assoc-in state [:dutch-piles] piles))

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
        (should= {:dutch-piles (repeat 8 [])
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
        (should= {:dutch-piles (repeat 12 [])
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
        (should= {:dutch-piles (repeat 16 [])
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
    (it "moves wood-pile back to hand"
      (let [[player :as players] [0 1]
            state (sut/init (count players) identity)
            empty-hand-state (add-to-wood-pile-until-empty-hand state player)
            reset-state (sut/reset-wood-pile empty-hand-state player)
            old-wood-pile (get-in empty-hand-state [:players player :wood-pile])]
        (should= (reverse old-wood-pile)
                 (get-in reset-state [:players player :hand]))
        (should= [] (get-in reset-state [:players player :wood-pile]))))

    (it "can not reset if hand is not empty"
      (let [[player :as players] [0 1]
            state (sut/init (count players) identity)]
        (should (sut/invalid-state? (sut/reset-wood-pile state player)))))

    )

  (context "cycling hand"
    (it "is not cyclable initially"
      (let [[player other-player :as players] [0 1]
            state (sut/init (count players) identity)]
        (should-not (sut/can-cycle-hand? state player))
        (should-not (sut/can-cycle-hand? state other-player))))

    (it "is not cyclable after going through your entire hand without resetting"
      (let [[player other-player :as players] [0 1]
            state (sut/init (count players) identity)
            empty-hand-state (add-to-wood-pile-until-empty-hand state player)]
        (should-not (sut/can-cycle-hand? empty-hand-state player))
        (should-not (sut/can-cycle-hand? empty-hand-state other-player))))

    (it "is cyclable only directly after resetting"
      (let [[player other-player :as players] [0 1]
            state (sut/init (count players) identity)
            empty-hand-state (add-to-wood-pile-until-empty-hand state player)
            reset-state (sut/reset-wood-pile empty-hand-state player)]
        (should (sut/can-cycle-hand? reset-state player))
        (should-not (sut/can-cycle-hand? reset-state other-player))))

    (it "is not cyclable before hand is empty"
      (let [[player other-player :as players] [0 1]
            state (sut/init (count players) identity)
            hand-states (add-to-wood-pile-until-empty-hand-states state player)]
        (should (every? not (map #(sut/can-cycle-hand? % player) hand-states)))
        (should (every? not (map #(sut/can-cycle-hand? % other-player) hand-states)))))
    )

  (context "moving blitz card to dutch pile"
    (it "creates new dutch pile from single card in blitz pile"
      (let [player 0
            card (->card 1 0 player)
            state (-> (sut/init 2 identity) (with-blitz-pile player [card]))
            result (sut/move-blitz->dutch state player 0)]
        (should= [] (get-in result [:players player :blitz-pile]))
        (should= [[card] [] [] [] [] [] [] []] (:dutch-piles result))))

    (it "removes only top card from blitz pile"
      (let [player 0
            card-1 (->card 1 0 player)
            card-2 (->card 2 1 player)
            card-3 (->card 3 2 player)
            state (-> (sut/init 2 identity)
                      (with-blitz-pile player [card-1 card-2 card-3]))
            result (sut/move-blitz->dutch state player 0)]
        (should= [card-2 card-3] (get-in result [:players player :blitz-pile]))
        (should= [[card-1] [] [] [] [] [] [] []] (:dutch-piles result))))

    (it "adds to second dutch pile"
      (let [player 0
            existing-card (->card 1 0 player)
            new-card (->card 1 1 player)
            state (-> (sut/init 2 identity)
                      (with-dutch-piles [[existing-card] []])
                      (with-blitz-pile player [new-card]))
            result (sut/move-blitz->dutch state player 1)]
        (should= [] (get-in result [:players player :blitz-pile]))
        (should= [[existing-card] [new-card]] (:dutch-piles result))))

    (it "adds consecutive card to dutch pile"
      (let [player 0
            existing-card (->card 1 0 player)
            new-card (->card 2 0 player)
            state (-> (sut/init 2 identity)
                      (with-dutch-piles [[existing-card]])
                      (with-blitz-pile player [new-card]))
            result (sut/move-blitz->dutch state player 0)]
        (should= [] (get-in result [:players player :blitz-pile]))
        (should= [[new-card existing-card]] (:dutch-piles result))))

    (it "returns invalid state if first card is not a 1"
      (let [player 0
            new-card (->card 2 0 player)
            state (-> (sut/init 2 identity)
                      (with-dutch-piles [[]])
                      (with-blitz-pile player [new-card]))
            result (sut/move-blitz->dutch state player 0)]
        (should (sut/invalid-state? result))))

    (it "returns invalid state if new card is not the same color as top card"
      (let [player 0
            existing-card (->card 1 0 player)
            new-card (->card 2 1 player)
            state (-> (sut/init 2 identity)
                      (with-dutch-piles [[existing-card]])
                      (with-blitz-pile player [new-card]))
            result (sut/move-blitz->dutch state player 0)]
        (should (sut/invalid-state? result))))

    (it "returns invalid state if cards are out of order"
      (let [player 0
            existing-card (->card 1 0 player)
            new-card (->card 3 0 player)
            state (-> (sut/init 2 identity)
                      (with-dutch-piles [[existing-card]])
                      (with-blitz-pile player [new-card]))
            result (sut/move-blitz->dutch state player 0)]
        (should (sut/invalid-state? result))))

    (it "returns invalid state if blitz pile is empty"
      (let [player 0
            state (-> (sut/init 2 identity)
                      (with-dutch-piles [[]])
                      (with-blitz-pile player []))
            result (sut/move-blitz->dutch state player 0)]
        (should (sut/invalid-state? result))))
     )

  (context "moving post card to dutch pile"
    (it "creates new dutch pile from single card in post pile"
      (let [player 0
            card (->card 1 0 player)
            state (-> (sut/init 2 identity) (with-post-piles player [[card] [] [] [] []]))
            result (sut/move-post->dutch state player 0 0)]
        (should= [[] [] [] [] []] (get-in result [:players player :post-piles]))
        (should= [[card] [] [] [] [] [] [] []] (:dutch-piles result))))

    (it "removes only top card from post pile"
      (let [player 0
            card-1 (->card 1 0 player)
            card-2 (->card 2 1 player)
            card-3 (->card 3 2 player)
            state (-> (sut/init 2 identity)
                      (with-post-piles player [[card-1 card-2 card-3] [] [] [] []]))
            result (sut/move-post->dutch state player 0 0)]
        (should= [[card-2 card-3] [] [] [] []] (get-in result [:players player :post-piles]))
        (should= [[card-1] [] [] [] [] [] [] []] (:dutch-piles result))))

    (it "adds to second dutch pile"
      (let [player 0
            existing-card (->card 1 0 player)
            new-card (->card 1 1 player)
            state (-> (sut/init 2 identity)
                      (with-dutch-piles [[existing-card] []])
                      (with-post-piles player [[new-card] [] [] [] []]))
            result (sut/move-post->dutch state player 0 1)]
        (should= [[] [] [] [] []] (get-in result [:players player :post-piles]))
        (should= [[existing-card] [new-card]] (:dutch-piles result))))

    (it "adds consecutive card to dutch pile"
      (let [player 0
            existing-card (->card 1 0 player)
            new-card (->card 2 0 player)
            state (-> (sut/init 2 identity)
                      (with-dutch-piles [[existing-card]])
                      (with-post-piles player [[new-card] [] [] [] []]))
            result (sut/move-post->dutch state player 0 0)]
        (should= [[] [] [] [] []] (get-in result [:players player :post-piles]))
        (should= [[new-card existing-card]] (:dutch-piles result))))

    (it "returns invalid state if first card is not a 1"
      (let [player 0
            new-card (->card 2 0 player)
            state (-> (sut/init 2 identity)
                      (with-dutch-piles [[]])
                      (with-post-piles player [[new-card] [] [] [] []]))
            result (sut/move-post->dutch state player 0 0)]
        (should (sut/invalid-state? result))))

    (it "returns invalid state if new card is not the same color as top card"
      (let [player 0
            existing-card (->card 1 0 player)
            new-card (->card 2 1 player)
            state (-> (sut/init 2 identity)
                      (with-dutch-piles [[existing-card]])
                      (with-post-piles player [[new-card] [] [] [] []]))
            result (sut/move-post->dutch state player 0 0)]
        (should (sut/invalid-state? result))))

    (it "returns invalid state if cards are out of order"
      (let [player 0
            existing-card (->card 1 0 player)
            new-card (->card 3 0 player)
            state (-> (sut/init 2 identity)
                      (with-dutch-piles [[existing-card]])
                      (with-post-piles player [[new-card] [] [] [] []]))
            result (sut/move-post->dutch state player 0 0)]
        (should (sut/invalid-state? result))))

    (it "returns invalid state if post pile is empty"
      (let [player 0
            state (-> (sut/init 2 identity)
                      (with-dutch-piles [[]])
                      (with-post-piles player [[] [] [] [] []]))
            result (sut/move-post->dutch state player 0 0)]
        (should (sut/invalid-state? result))))

    (it "moves card from non-zero post pile"
      (let [player 0
            card (->card 1 0 player)
            state (-> (sut/init 2 identity) (with-post-piles player [[] [] [card] [] []]))
            result (sut/move-post->dutch state player 2 0)]
        (should= [[] [] [] [] []] (get-in result [:players player :post-piles]))
        (should= [[card] [] [] [] [] [] [] []] (:dutch-piles result))))

    (it "moves card from player 1 post pile"
      (let [player-0 0
            player-1 1
            card (->card 1 0 player-1)
            player-0-post-piles [[(->card 5 1 player-0)] [] [] [] []]
            state (-> (sut/init 2 identity)
                      (with-post-piles player-0 player-0-post-piles)
                      (with-post-piles player-1 [[card] [] [] [] []]))
            result (sut/move-post->dutch state player-1 0 0)]
        (should= [[] [] [] [] []] (get-in result [:players player-1 :post-piles]))
        (should= player-0-post-piles (get-in result [:players player-0 :post-piles]))
        (should= [[card] [] [] [] [] [] [] []] (:dutch-piles result))))
    )

  ; add to wood pile (done - may need to add rule for when 1 or 2 cards are left in hand)
  ; reset hand (done)
  ; cycle hand (WIP)
  ; move card to dutch pile (WIP)
  ; move card to post pile

  )