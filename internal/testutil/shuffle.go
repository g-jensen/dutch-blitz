package testutil

import "dutch_blitz/internal/game"

func Identity(cards []game.Card) []game.Card {
	return cards
}

func RotateOne(cards []game.Card) []game.Card {
	if len(cards) == 0 {
		return cards
	}
	return append(cards[1:], cards[0])
}
