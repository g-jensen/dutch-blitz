package play

import (
	"dutch_blitz/internal/game"
	memorygame "dutch_blitz/internal/game/memorygame"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func identity(cards []game.Card) []game.Card {
	return cards
}

func rotateOne(cards []game.Card) []game.Card {
	if len(cards) == 0 {
		return cards
	}
	return append(cards[1:], cards[0])
}

func setup(playerCount int, shuffleFn func([]game.Card) []game.Card) (game.Game, error) {
	g := memorygame.New(playerCount)
	err := Setup(g, playerCount, shuffleFn)
	return g, err
}

func TestInit_ReturnsError_WhenZeroPlayers(t *testing.T) {
	_, err := setup(0, nil)

	assert.Error(t, err)
}

func TestInit_ReturnsError_WhenFivePlayers(t *testing.T) {
	_, err := setup(5, nil)

	assert.Error(t, err)
}

func TestInit_ReturnsTwoPlayers(t *testing.T) {
	s, err := setup(2, identity)

	require.NoError(t, err)
	assert.Len(t, s.Players(), 2)
}

func TestInit_ReturnsEightDutchPiles(t *testing.T) {
	s, err := setup(2, identity)

	require.NoError(t, err)
	assert.Len(t, s.DutchPiles(), 8)
}

func TestInit_TwoPlayers_EachHaveFivePostPiles(t *testing.T) {
	s, err := setup(2, identity)

	require.NoError(t, err)
	assert.Len(t, s.PostPiles(game.Player(0)), 5)
}

func TestInit_TwoPlayers_BlitzPileHasTenCards(t *testing.T) {
	s, err := setup(2, identity)

	require.NoError(t, err)
	assert.Len(t, s.BlitzPile(game.Player(0)), 10)
	assert.Equal(t, game.Card{Number: 6, Color: 0, Player: 0}, s.BlitzPile(game.Player(0))[0])
}

func TestInit_TwoPlayers_HandHas25Cards(t *testing.T) {
	s, err := setup(2, identity)

	require.NoError(t, err)
	assert.Len(t, s.Hand(game.Player(0)), 25)
	assert.Equal(t, game.Card{Number: 6, Color: 1, Player: 0}, s.Hand(game.Player(0))[0])
}

func TestInit_TwoPlayers_PostPileHasOneCard(t *testing.T) {
	s, err := setup(2, identity)

	require.NoError(t, err)
	assert.Len(t, s.PostPiles(game.Player(0))[0], 1)
	assert.Equal(t, game.Card{Number: 1, Color: 0, Player: 0}, s.PostPiles(game.Player(0))[0][0])
}

func TestInit_ThreePlayers_EachHaveThreePostPiles(t *testing.T) {
	s, err := setup(3, identity)

	require.NoError(t, err)
	assert.Len(t, s.PostPiles(game.Player(0)), 3)
}

func TestInit_ShufflesEachPlayerDeck(t *testing.T) {
	s, err := setup(2, rotateOne)

	require.NoError(t, err)
	firstPostPile := s.PostPiles(game.Player(0))[0]
	assert.Equal(t, 2, firstPostPile[0].Number)
}

func TestAddToWoodPile_MovesTop3CardsFromHandToWoodPile(t *testing.T) {
	s, err := setup(2, identity)
	require.NoError(t, err)
	player := game.Player(0)

	AddToWoodPile(s, player)

	assert.Len(t, s.WoodPile(player), 3)
	assert.Len(t, s.Hand(player), 22)
	assert.Equal(t, game.Card{Number: 8, Color: 1, Player: 0}, s.WoodPile(player)[0])
	assert.Equal(t, game.Card{Number: 7, Color: 1, Player: 0}, s.WoodPile(player)[1])
	assert.Equal(t, game.Card{Number: 6, Color: 1, Player: 0}, s.WoodPile(player)[2])
}

func TestAddToWoodPile_MovesAllCards_WhenHandHasFewerThan3(t *testing.T) {
	s, err := setup(2, identity)
	require.NoError(t, err)
	player := game.Player(0)
	s.SetHand(player, []game.Card{{Number: 1}, {Number: 2}})

	err = AddToWoodPile(s, player)

	require.NoError(t, err)
	assert.Len(t, s.WoodPile(player), 2)
	assert.Len(t, s.Hand(player), 0)
	assert.Equal(t, game.Card{Number: 2}, s.WoodPile(player)[0])
	assert.Equal(t, game.Card{Number: 1}, s.WoodPile(player)[1])
}

func TestResetWoodPile_MovesWoodPileToHand(t *testing.T) {
	s, err := setup(2, identity)
	require.NoError(t, err)
	player := game.Player(0)
	woodCards := []game.Card{{Number: 1}, {Number: 2}, {Number: 3}}
	s.SetHand(player, []game.Card{})
	s.SetWoodPile(player, woodCards)

	err = ResetWoodPile(s, player)

	require.NoError(t, err)
	assert.Len(t, s.Hand(player), 3)
	assert.Len(t, s.WoodPile(player), 0)
	assert.Equal(t, game.Card{Number: 3}, s.Hand(player)[0])
	assert.Equal(t, game.Card{Number: 2}, s.Hand(player)[1])
	assert.Equal(t, game.Card{Number: 1}, s.Hand(player)[2])
}

func TestResetWoodPile_ReturnsError_WhenHandNotEmpty(t *testing.T) {
	s, err := setup(2, identity)
	require.NoError(t, err)
	player := game.Player(0)
	s.SetWoodPile(player, []game.Card{{Number: 1}, {Number: 2}, {Number: 3}})

	err = ResetWoodPile(s, player)

	assert.Error(t, err)
}

func TestBlitzToDutch_MovesTopBlitzCardToDutchPile(t *testing.T) {
	s, err := setup(2, identity)
	require.NoError(t, err)
	player := game.Player(0)
	s.SetBlitzPile(player, []game.Card{{Number: 1, Color: 0, Player: player}})

	err = BlitzToDutch(s, player, 0)

	require.NoError(t, err)
	assert.Len(t, s.DutchPiles()[0], 1)
	assert.Len(t, s.BlitzPile(player), 0)
	assert.Equal(t, game.Card{Number: 1, Color: 0, Player: player}, s.DutchPiles()[0][0])
}

func TestBlitzToDutch_ReturnsError_WhenPlayingNonOneToEmptyPile(t *testing.T) {
	s, err := setup(2, identity)
	require.NoError(t, err)
	player := game.Player(0)
	s.SetBlitzPile(player, []game.Card{{Number: 2, Color: 0, Player: player}})

	err = BlitzToDutch(s, player, 0)

	assert.Error(t, err)
}

func TestBlitzToDutch_ReturnsError_WhenCardIsNotNextConsecutiveNumber(t *testing.T) {
	s, err := setup(2, identity)
	require.NoError(t, err)
	player := game.Player(0)
	dutchPiles := s.DutchPiles()
	dutchPiles[0] = []game.Card{{Number: 1, Color: 0, Player: player}}
	s.SetBlitzPile(player, []game.Card{{Number: 3, Color: 0, Player: player}})

	err = BlitzToDutch(s, player, 0)

	assert.Error(t, err)
}

func TestBlitzToDutch_ReturnsError_WhenCardColorDoesNotMatchPile(t *testing.T) {
	s, err := setup(2, identity)
	require.NoError(t, err)
	player := game.Player(0)
	dutchPiles := s.DutchPiles()
	dutchPiles[0] = []game.Card{{Number: 1, Color: 0, Player: player}}
	s.SetBlitzPile(player, []game.Card{{Number: 2, Color: 1, Player: player}})

	err = BlitzToDutch(s, player, 0)

	assert.Error(t, err)
}

func TestWoodPileToDutch_MovesTopWoodCardToDutchPile(t *testing.T) {
	s, err := setup(2, identity)
	require.NoError(t, err)
	player := game.Player(0)
	s.SetWoodPile(player, []game.Card{{Number: 1, Color: 0, Player: player}})

	err = WoodPileToDutch(s, player, 0)

	require.NoError(t, err)
	assert.Len(t, s.DutchPiles()[0], 1)
	assert.Len(t, s.WoodPile(player), 0)
	assert.Equal(t, game.Card{Number: 1, Color: 0, Player: player}, s.DutchPiles()[0][0])
}

func TestPostToDutch_MovesTopPostCardToDutchPile(t *testing.T) {
	s, err := setup(2, identity)
	require.NoError(t, err)
	player := game.Player(0)
	postPiles := s.PostPiles(player)
	postPiles[0] = []game.Card{{Number: 1, Color: 0, Player: player}}
	s.SetPostPiles(player, postPiles)

	err = PostToDutch(s, player, 0, 0)

	require.NoError(t, err)
	assert.Len(t, s.DutchPiles()[0], 1)
	assert.Len(t, s.PostPiles(player)[0], 0)
	assert.Equal(t, game.Card{Number: 1, Color: 0, Player: player}, s.DutchPiles()[0][0])
}

func TestWoodPileToDutch_ReturnsError_WhenPlayingNonOneToEmptyPile(t *testing.T) {
	s, err := setup(2, identity)
	require.NoError(t, err)
	player := game.Player(0)
	s.SetWoodPile(player, []game.Card{{Number: 2, Color: 0, Player: player}})

	err = WoodPileToDutch(s, player, 0)

	assert.Error(t, err)
}

func TestWoodPileToDutch_ReturnsError_WhenCardIsNotNextConsecutiveNumber(t *testing.T) {
	s, err := setup(2, identity)
	require.NoError(t, err)
	player := game.Player(0)
	dutchPiles := s.DutchPiles()
	dutchPiles[0] = []game.Card{{Number: 1, Color: 0, Player: player}}
	s.SetWoodPile(player, []game.Card{{Number: 3, Color: 0, Player: player}})

	err = WoodPileToDutch(s, player, 0)

	assert.Error(t, err)
}

func TestWoodPileToDutch_ReturnsError_WhenCardColorDoesNotMatchPile(t *testing.T) {
	s, err := setup(2, identity)
	require.NoError(t, err)
	player := game.Player(0)
	dutchPiles := s.DutchPiles()
	dutchPiles[0] = []game.Card{{Number: 1, Color: 0, Player: player}}
	s.SetWoodPile(player, []game.Card{{Number: 2, Color: 1, Player: player}})

	err = WoodPileToDutch(s, player, 0)

	assert.Error(t, err)
}

func TestPostToDutch_ReturnsError_WhenPlayingNonOneToEmptyPile(t *testing.T) {
	s, err := setup(2, identity)
	require.NoError(t, err)
	player := game.Player(0)
	postPiles := s.PostPiles(player)
	postPiles[0] = []game.Card{{Number: 2, Color: 0, Player: player}}
	s.SetPostPiles(player, postPiles)

	err = PostToDutch(s, player, 0, 0)

	assert.Error(t, err)
}

func TestPostToDutch_ReturnsError_WhenCardIsNotNextConsecutiveNumber(t *testing.T) {
	s, err := setup(2, identity)
	require.NoError(t, err)
	player := game.Player(0)
	dutchPiles := s.DutchPiles()
	dutchPiles[0] = []game.Card{{Number: 1, Color: 0, Player: player}}
	postPiles := s.PostPiles(player)
	postPiles[0] = []game.Card{{Number: 3, Color: 0, Player: player}}
	s.SetPostPiles(player, postPiles)

	err = PostToDutch(s, player, 0, 0)

	assert.Error(t, err)
}

func TestPostToDutch_ReturnsError_WhenCardColorDoesNotMatchPile(t *testing.T) {
	s, err := setup(2, identity)
	require.NoError(t, err)
	player := game.Player(0)
	dutchPiles := s.DutchPiles()
	dutchPiles[0] = []game.Card{{Number: 1, Color: 0, Player: player}}
	postPiles := s.PostPiles(player)
	postPiles[0] = []game.Card{{Number: 2, Color: 1, Player: player}}
	s.SetPostPiles(player, postPiles)

	err = PostToDutch(s, player, 0, 0)

	assert.Error(t, err)
}

func TestBlitzToPost_MovesTopBlitzCardToPostPile(t *testing.T) {
	s, err := setup(2, identity)
	require.NoError(t, err)
	player := game.Player(0)
	s.SetBlitzPile(player, []game.Card{{Number: 5, Color: 0, Player: player}})
	postPiles := s.PostPiles(player)
	postPiles[0] = []game.Card{}
	s.SetPostPiles(player, postPiles)

	err = BlitzToPost(s, player, 0)

	require.NoError(t, err)
	assert.Len(t, s.PostPiles(player)[0], 1)
	assert.Len(t, s.BlitzPile(player), 0)
	assert.Equal(t, game.Card{Number: 5, Color: 0, Player: player}, s.PostPiles(player)[0][0])
}

func TestBlitzToPost_ReturnsError_WhenCardIsNotNextDescendingNumber(t *testing.T) {
	s, err := setup(2, identity)
	require.NoError(t, err)
	player := game.Player(0)
	postPiles := s.PostPiles(player)
	postPiles[0] = []game.Card{{Number: 5, Color: 0, Player: player}}
	s.SetPostPiles(player, postPiles)
	s.SetBlitzPile(player, []game.Card{{Number: 3, Color: 0, Player: player}})

	err = BlitzToPost(s, player, 0)

	assert.Error(t, err)
}

func TestBlitzToPost_ReturnsError_WhenColorDoesNotAlternate(t *testing.T) {
	s, err := setup(2, identity)
	require.NoError(t, err)
	player := game.Player(0)
	postPiles := s.PostPiles(player)
	postPiles[0] = []game.Card{{Number: 5, Color: 0, Player: player}}
	s.SetPostPiles(player, postPiles)
	s.SetBlitzPile(player, []game.Card{{Number: 4, Color: 2, Player: player}})

	err = BlitzToPost(s, player, 0)

	assert.Error(t, err)
}

func TestWoodPileToPost_MovesTopWoodCardToPostPile(t *testing.T) {
	s, err := setup(2, identity)
	require.NoError(t, err)
	player := game.Player(0)
	s.SetWoodPile(player, []game.Card{{Number: 5, Color: 0, Player: player}})
	postPiles := s.PostPiles(player)
	postPiles[0] = []game.Card{}
	s.SetPostPiles(player, postPiles)

	err = WoodPileToPost(s, player, 0)

	require.NoError(t, err)
	assert.Len(t, s.PostPiles(player)[0], 1)
	assert.Len(t, s.WoodPile(player), 0)
	assert.Equal(t, game.Card{Number: 5, Color: 0, Player: player}, s.PostPiles(player)[0][0])
}

func TestWoodPileToPost_ReturnsError_WhenCardIsNotNextDescendingNumber(t *testing.T) {
	s, err := setup(2, identity)
	require.NoError(t, err)
	player := game.Player(0)
	postPiles := s.PostPiles(player)
	postPiles[0] = []game.Card{{Number: 5, Color: 0, Player: player}}
	s.SetPostPiles(player, postPiles)
	s.SetWoodPile(player, []game.Card{{Number: 3, Color: 1, Player: player}})

	err = WoodPileToPost(s, player, 0)

	assert.Error(t, err)
}

func TestWoodPileToPost_ReturnsError_WhenColorDoesNotAlternate(t *testing.T) {
	s, err := setup(2, identity)
	require.NoError(t, err)
	player := game.Player(0)
	postPiles := s.PostPiles(player)
	postPiles[0] = []game.Card{{Number: 5, Color: 0, Player: player}}
	s.SetPostPiles(player, postPiles)
	s.SetWoodPile(player, []game.Card{{Number: 4, Color: 2, Player: player}})

	err = WoodPileToPost(s, player, 0)

	assert.Error(t, err)
}

func TestPostToPost_MovesTopCardFromOnePostPileToAnother(t *testing.T) {
	s, err := setup(2, identity)
	require.NoError(t, err)
	player := game.Player(0)
	postPiles := s.PostPiles(player)
	postPiles[0] = []game.Card{{Number: 5, Color: 0, Player: player}}
	postPiles[1] = []game.Card{}
	s.SetPostPiles(player, postPiles)

	err = PostToPost(s, player, 0, 1)

	require.NoError(t, err)
	assert.Len(t, s.PostPiles(player)[0], 0)
	assert.Len(t, s.PostPiles(player)[1], 1)
	assert.Equal(t, 5, s.PostPiles(player)[1][0].Number)
}

func TestPostToPost_ReturnsError_WhenCardIsNotNextDescendingNumber(t *testing.T) {
	s, err := setup(2, identity)
	require.NoError(t, err)
	player := game.Player(0)
	postPiles := s.PostPiles(player)
	postPiles[1] = []game.Card{{Number: 5, Color: 0, Player: player}}
	postPiles[0] = []game.Card{{Number: 3, Color: 1, Player: player}}
	s.SetPostPiles(player, postPiles)

	err = PostToPost(s, player, 0, 1)

	assert.Error(t, err)
}

func TestPostToPost_ReturnsError_WhenColorDoesNotAlternate(t *testing.T) {
	s, err := setup(2, identity)
	require.NoError(t, err)
	player := game.Player(0)
	postPiles := s.PostPiles(player)
	postPiles[1] = []game.Card{{Number: 5, Color: 0, Player: player}}
	postPiles[0] = []game.Card{{Number: 4, Color: 2, Player: player}}
	s.SetPostPiles(player, postPiles)

	err = PostToPost(s, player, 0, 1)

	assert.Error(t, err)
}

func TestResetWoodPile_RestoresHandToInitialState_AfterEmptyingHand(t *testing.T) {
	s, err := setup(2, identity)
	require.NoError(t, err)
	player := game.Player(0)
	initialHand := s.Hand(player)

	for len(s.Hand(player)) > 0 {
		err = AddToWoodPile(s, player)
		require.NoError(t, err)
	}

	err = ResetWoodPile(s, player)

	require.NoError(t, err)
	assert.Equal(t, initialHand, s.Hand(player))
	assert.Empty(t, s.WoodPile(player))
}

func TestCycleHand_MovesFirstCardToEnd(t *testing.T) {
	s, err := setup(2, identity)
	require.NoError(t, err)
	player := game.Player(0)
	s.SetCyclable(player, true)
	firstCard := s.Hand(player)[0]
	handLen := len(s.Hand(player))

	err = CycleHand(s, player)

	require.NoError(t, err)
	assert.Len(t, s.Hand(player), handLen)
	assert.Equal(t, firstCard, s.Hand(player)[handLen-1])
}

func TestCycleHand_ReturnsError_WhenPlayerNotCyclable(t *testing.T) {
	s, err := setup(2, identity)
	require.NoError(t, err)
	player := game.Player(0)

	err = CycleHand(s, player)

	assert.Error(t, err)
}

func TestResetWoodPile_MakesPlayerCyclable(t *testing.T) {
	s, err := setup(2, identity)
	require.NoError(t, err)
	player := game.Player(0)

	for len(s.Hand(player)) > 0 {
		err = AddToWoodPile(s, player)
		require.NoError(t, err)
	}

	err = ResetWoodPile(s, player)

	require.NoError(t, err)
	assert.True(t, s.IsCyclable(player))
}

func TestCycleHand_ReturnsError_AfterPlayingCardSinceReset(t *testing.T) {
	s, err := setup(2, identity)
	require.NoError(t, err)
	player := game.Player(0)

	for len(s.Hand(player)) > 0 {
		err = AddToWoodPile(s, player)
		require.NoError(t, err)
	}
	err = ResetWoodPile(s, player)
	require.NoError(t, err)

	s.SetBlitzPile(player, []game.Card{{Number: 1, Color: 0, Player: player}})
	err = BlitzToDutch(s, player, 0)
	require.NoError(t, err)

	err = CycleHand(s, player)

	assert.Error(t, err)
}

func TestCycleHand_Succeeds_AfterResettingTwice(t *testing.T) {
	s, err := setup(2, identity)
	require.NoError(t, err)
	player := game.Player(0)

	for len(s.Hand(player)) > 0 {
		err = AddToWoodPile(s, player)
		require.NoError(t, err)
	}
	err = ResetWoodPile(s, player)
	require.NoError(t, err)

	s.SetBlitzPile(player, []game.Card{{Number: 1, Color: 0, Player: player}})
	err = BlitzToDutch(s, player, 0)
	require.NoError(t, err)

	for len(s.Hand(player)) > 0 {
		err = AddToWoodPile(s, player)
		require.NoError(t, err)
	}
	err = ResetWoodPile(s, player)
	require.NoError(t, err)

	err = CycleHand(s, player)

	assert.NoError(t, err)
}

func TestCycleHand_ReturnsError_AfterPostToDutch(t *testing.T) {
	s, err := setup(2, identity)
	require.NoError(t, err)
	player := game.Player(0)

	for len(s.Hand(player)) > 0 {
		err = AddToWoodPile(s, player)
		require.NoError(t, err)
	}
	err = ResetWoodPile(s, player)
	require.NoError(t, err)

	postPiles := s.PostPiles(player)
	postPiles[0] = []game.Card{{Number: 1, Color: 0, Player: player}}
	s.SetPostPiles(player, postPiles)
	err = PostToDutch(s, player, 0, 0)
	require.NoError(t, err)

	err = CycleHand(s, player)

	assert.Error(t, err)
}

func TestCycleHand_ReturnsError_AfterWoodPileToDutch(t *testing.T) {
	s, err := setup(2, identity)
	require.NoError(t, err)
	player := game.Player(0)

	for len(s.Hand(player)) > 0 {
		err = AddToWoodPile(s, player)
		require.NoError(t, err)
	}
	err = ResetWoodPile(s, player)
	require.NoError(t, err)

	s.SetWoodPile(player, []game.Card{{Number: 1, Color: 0, Player: player}})
	err = WoodPileToDutch(s, player, 0)
	require.NoError(t, err)

	err = CycleHand(s, player)

	assert.Error(t, err)
}

func TestCycleHand_ReturnsError_AfterBlitzToPost(t *testing.T) {
	s, err := setup(2, identity)
	require.NoError(t, err)
	player := game.Player(0)

	for len(s.Hand(player)) > 0 {
		err = AddToWoodPile(s, player)
		require.NoError(t, err)
	}
	err = ResetWoodPile(s, player)
	require.NoError(t, err)

	s.SetBlitzPile(player, []game.Card{{Number: 5, Color: 0, Player: player}})
	postPiles := s.PostPiles(player)
	postPiles[0] = []game.Card{}
	s.SetPostPiles(player, postPiles)
	err = BlitzToPost(s, player, 0)
	require.NoError(t, err)

	err = CycleHand(s, player)

	assert.Error(t, err)
}

func TestCycleHand_ReturnsError_AfterWoodPileToPost(t *testing.T) {
	s, err := setup(2, identity)
	require.NoError(t, err)
	player := game.Player(0)

	for len(s.Hand(player)) > 0 {
		err = AddToWoodPile(s, player)
		require.NoError(t, err)
	}
	err = ResetWoodPile(s, player)
	require.NoError(t, err)

	s.SetWoodPile(player, []game.Card{{Number: 5, Color: 0, Player: player}})
	postPiles := s.PostPiles(player)
	postPiles[0] = []game.Card{}
	s.SetPostPiles(player, postPiles)
	err = WoodPileToPost(s, player, 0)
	require.NoError(t, err)

	err = CycleHand(s, player)

	assert.Error(t, err)
}

func TestCycleHand_ReturnsError_AfterPostToPost(t *testing.T) {
	s, err := setup(2, identity)
	require.NoError(t, err)
	player := game.Player(0)

	for len(s.Hand(player)) > 0 {
		err = AddToWoodPile(s, player)
		require.NoError(t, err)
	}
	err = ResetWoodPile(s, player)
	require.NoError(t, err)

	postPiles := s.PostPiles(player)
	postPiles[0] = []game.Card{{Number: 5, Color: 0, Player: player}}
	postPiles[1] = []game.Card{}
	s.SetPostPiles(player, postPiles)
	err = PostToPost(s, player, 0, 1)
	require.NoError(t, err)

	err = CycleHand(s, player)

	assert.Error(t, err)
}

func TestHasWon_ReturnsTrue_WhenBlitzPileEmpty(t *testing.T) {
	s, err := setup(2, identity)
	require.NoError(t, err)
	player := game.Player(0)
	s.SetBlitzPile(player, []game.Card{})

	result := HasWon(s, player)

	assert.True(t, result)
}

func TestHasWon_ReturnsFalse_WhenBlitzPileNotEmpty(t *testing.T) {
	s, err := setup(2, identity)
	require.NoError(t, err)
	player := game.Player(0)

	result := HasWon(s, player)

	assert.False(t, result)
}

func TestBlitzToDutch_ReturnsError_WhenBlitzPileIsEmpty(t *testing.T) {
	s, err := setup(2, identity)
	require.NoError(t, err)
	player := game.Player(0)
	s.SetBlitzPile(player, []game.Card{})

	err = BlitzToDutch(s, player, 0)

	assert.Error(t, err)
}

func TestPostToDutch_ReturnsError_WhenPostPileIsEmpty(t *testing.T) {
	s, err := setup(2, identity)
	require.NoError(t, err)
	player := game.Player(0)
	postPiles := s.PostPiles(player)
	postPiles[0] = []game.Card{}
	s.SetPostPiles(player, postPiles)

	err = PostToDutch(s, player, 0, 0)

	assert.Error(t, err)
}

func TestWoodPileToDutch_ReturnsError_WhenWoodPileIsEmpty(t *testing.T) {
	s, err := setup(2, identity)
	require.NoError(t, err)
	player := game.Player(0)

	err = WoodPileToDutch(s, player, 0)

	assert.Error(t, err)
}

func TestBlitzToPost_ReturnsError_WhenBlitzPileIsEmpty(t *testing.T) {
	s, err := setup(2, identity)
	require.NoError(t, err)
	player := game.Player(0)
	s.SetBlitzPile(player, []game.Card{})
	postPiles := s.PostPiles(player)
	postPiles[0] = []game.Card{}
	s.SetPostPiles(player, postPiles)

	err = BlitzToPost(s, player, 0)

	assert.Error(t, err)
}

func TestWoodPileToPost_ReturnsError_WhenWoodPileIsEmpty(t *testing.T) {
	s, err := setup(2, identity)
	require.NoError(t, err)
	player := game.Player(0)

	err = WoodPileToPost(s, player, 0)

	assert.Error(t, err)
}

func TestPostToPost_ReturnsError_WhenSourcePostPileIsEmpty(t *testing.T) {
	s, err := setup(2, identity)
	require.NoError(t, err)
	player := game.Player(0)
	postPiles := s.PostPiles(player)
	postPiles[0] = []game.Card{}
	s.SetPostPiles(player, postPiles)

	err = PostToPost(s, player, 0, 1)

	assert.Error(t, err)
}
