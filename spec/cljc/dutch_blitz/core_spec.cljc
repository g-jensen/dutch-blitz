(ns dutch-blitz.core-spec
  (:require [speclj.core #?(:clj :refer :cljs :refer-macros) [around context describe it should= should-be-nil should-contain should should-not before with-stubs]]
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

(describe "Dutch Blitz"

  (it "has a deck"
    (let [deck (for [n (range 0 10)
                     type (range 0 4)]
                 {:number n :type type})]
      (should= (map #(assoc % :player 0) deck)
               (sut/->deck 0))
      (should= (map #(assoc % :player 1) deck)
               (sut/->deck 1))))

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
                          (sut/add-to-wood-pile state player)))))

  #_(context "cyclable deck"
             #_(it "isn't cyclable initially"
                   (let [state (sut/init 2 identity)]
                     (should-not (some identity (map :cyclable? (:players state))))))

             (it "is cyclable after going through your entire hand and not playing"
                 (let [state (sut/init 2 identity)
                       states (iterate #(sut/add-to-wood-pile % 0) state)
                       empty-hand-state (first (drop-while #(seq (get-in % [:players 0 :hand])) states))])))

  )