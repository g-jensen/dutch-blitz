package testutil

import (
	"dutch_blitz/internal/game"
	"dutch_blitz/internal/play"
)

func EmptyHand(g game.Game, player game.Player) {
	for len(g.Hand(player)) > 0 {
		play.AddToWoodPile(g, player)
	}
}

func EmptyHandAndResetWoodPile(g game.Game, player game.Player) error {
	EmptyHand(g, player)
	return play.ResetWoodPile(g, player)
}
