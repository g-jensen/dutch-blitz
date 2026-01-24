package testutil

import (
	"dutch_blitz/internal/game"
	"dutch_blitz/internal/game/memorygame"
	"dutch_blitz/internal/play"
	"testing"

	"github.com/stretchr/testify/require"
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

func GameSetupShuffle(playerCount int, shuffleFn play.ShuffleFn) (game.Game, error) {
	g := memorygame.New(playerCount)
	err := play.Setup(g, playerCount, shuffleFn)
	return g, err
}

func GameSetup(playerCount int) (game.Game, error) {
	return GameSetupShuffle(playerCount, Identity)
}

func GameSetupShuffleNoError(playerCount int, shuffleFn play.ShuffleFn, t *testing.T) game.Game {
	g, err := GameSetupShuffle(playerCount, shuffleFn)
	require.NoError(t, err)
	return g
}

func GameSetupNoError(playerCount int, t *testing.T) game.Game {
	g, err := GameSetup(playerCount)
	require.NoError(t, err)
	return g
}
