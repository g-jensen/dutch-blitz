package playtest

import (
	"dutch_blitz/internal/game"
	sut "dutch_blitz/internal/play"
	"dutch_blitz/internal/play/errors"
	"dutch_blitz/internal/testutil"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

const minPlayers = 2
const maxPlayers = 4

func TestTopCard_ReturnsEmptyCardIfNoCards(t *testing.T) {
	assert.Equal(t, game.Card{}, sut.TopCard([]game.Card{}))
}

func TestTopCard_ReturnsCard(t *testing.T) {
	card := game.Card{Number: 0, Color: 0, Player: 0}
	assert.Equal(t, card, sut.TopCard([]game.Card{card}))
}

func TestTopCard_ReturnsLastCard(t *testing.T) {
	card1 := game.Card{Number: 0, Color: 0, Player: 0}
	card2 := game.Card{Number: 1, Color: 0, Player: 0}
	assert.Equal(t, card2, sut.TopCard([]game.Card{card1, card2}))
}

func TestInit_ReturnsError_WhenZeroPlayers(t *testing.T) {
	_, err := testutil.GameSetup(0)
	assert.Error(t, err)
	assert.Equal(t, errors.BadPlayerCount(0, minPlayers, maxPlayers), err)
}

func TestInit_ReturnsError_WhenFivePlayers(t *testing.T) {
	_, err := testutil.GameSetup(5)
	assert.Error(t, err)
	assert.Equal(t, errors.BadPlayerCount(5, minPlayers, maxPlayers), err)
}

func TestInit_ReturnsError_WhenOnePlayer(t *testing.T) {
	_, err := testutil.GameSetup(1)
	assert.Error(t, err)
	assert.Equal(t, errors.BadPlayerCount(1, minPlayers, maxPlayers), err)
}

func TestInit_ReturnsTwoPlayers(t *testing.T) {
	g := testutil.GameSetupNoError(2, t)
	assert.Len(t, g.Players(), 2)
}

func TestInit_ReturnsEightDutchPiles(t *testing.T) {
	g := testutil.GameSetupNoError(2, t)
	assert.Len(t, g.DutchPiles(), 8)
}

func TestInit_TwoPlayers_EachHaveFivePostPiles(t *testing.T) {
	g := testutil.GameSetupNoError(2, t)
	assert.Len(t, g.PostPiles(game.Player(0)), 5)
}

func TestInit_TwoPlayers_BlitzPileHasTenCards(t *testing.T) {
	g := testutil.GameSetupNoError(2, t)
	assert.Len(t, g.BlitzPile(game.Player(0)), 10)
	assert.Equal(t, game.Card{Number: 6, Color: 0, Player: 0}, g.BlitzPile(game.Player(0))[0])
}

func TestInit_TwoPlayers_HandHas25Cards(t *testing.T) {
	g := testutil.GameSetupNoError(2, t)
	assert.Len(t, g.Hand(game.Player(0)), 25)
	assert.Equal(t, game.Card{Number: 6, Color: 1, Player: 0}, g.Hand(game.Player(0))[0])
}

func TestInit_TwoPlayers_PostPileHasOneCard(t *testing.T) {
	g := testutil.GameSetupNoError(2, t)
	assert.Len(t, g.PostPiles(game.Player(0))[0], 1)
	assert.Equal(t, game.Card{Number: 1, Color: 0, Player: 0}, g.PostPiles(game.Player(0))[0][0])
}

func TestInit_ThreePlayers_EachHaveThreePostPiles(t *testing.T) {
	g := testutil.GameSetupNoError(3, t)
	assert.Len(t, g.PostPiles(game.Player(0)), 3)
}

func TestInit_ShufflesEachPlayerDeck(t *testing.T) {
	g := testutil.GameSetupShuffleNoError(2, testutil.RotateOne, t)
	firstPostPile := g.PostPiles(game.Player(0))[0]
	assert.Equal(t, 2, firstPostPile[0].Number)
}

func TestAddToWoodPile_MovesTop3CardsFromHandToWoodPile(t *testing.T) {
	g := testutil.GameSetupNoError(2, t)
	player := game.Player(0)

	sut.AddToWoodPile(g, player)

	assert.Len(t, g.WoodPile(player), 3)
	assert.Len(t, g.Hand(player), 22)
	assert.Equal(t, game.Card{Number: 6, Color: 1, Player: 0}, g.WoodPile(player)[0])
	assert.Equal(t, game.Card{Number: 7, Color: 1, Player: 0}, g.WoodPile(player)[1])
	assert.Equal(t, game.Card{Number: 8, Color: 1, Player: 0}, g.WoodPile(player)[2])
}

func TestAddToWoodPile_MovesAllCards_WhenHandHasFewerThan3(t *testing.T) {
	g := testutil.GameSetupNoError(2, t)
	player := game.Player(0)
	g.SetHand(player, []game.Card{{Number: 1}, {Number: 2}})

	sut.AddToWoodPile(g, player)

	assert.Len(t, g.WoodPile(player), 2)
	assert.Len(t, g.Hand(player), 0)
	assert.Equal(t, game.Card{Number: 1}, g.WoodPile(player)[0])
	assert.Equal(t, game.Card{Number: 2}, g.WoodPile(player)[1])
}

func TestResetWoodPile_TransfersWithoutReversing(t *testing.T) {
	g := testutil.GameSetupNoError(2, t)
	player := game.Player(0)
	woodCards := []game.Card{
		{Number: 1, Color: 0, Player: player},
		{Number: 2, Color: 0, Player: player},
		{Number: 3, Color: 0, Player: player},
	}
	g.SetWoodPile(player, woodCards)
	g.SetHand(player, []game.Card{})

	err := sut.ResetWoodPile(g, player)

	require.NoError(t, err)
	hand := g.Hand(player)
	assert.Equal(t, 1, hand[0].Number)
	assert.Equal(t, 2, hand[1].Number)
	assert.Equal(t, 3, hand[2].Number)
}

func TestResetWoodPile_ReturnsError_WhenHandNotEmpty(t *testing.T) {
	g := testutil.GameSetupNoError(2, t)
	player := game.Player(0)
	g.SetWoodPile(player, []game.Card{{Number: 1}, {Number: 2}, {Number: 3}})

	err := sut.ResetWoodPile(g, player)

	assert.Error(t, err)
	assert.Equal(t, errors.BadAction(), err)
}

func TestBlitzToDutch_MovesTopBlitzCardToDutchPile(t *testing.T) {
	g := testutil.GameSetupNoError(2, t)
	player := game.Player(0)
	g.SetBlitzPile(player, []game.Card{{Number: 1, Color: 0, Player: player}})

	err := sut.BlitzToDutch(g, player, 0)

	require.NoError(t, err)
	assert.Len(t, g.DutchPiles()[0], 1)
	assert.Len(t, g.BlitzPile(player), 0)
	assert.Equal(t, game.Card{Number: 1, Color: 0, Player: player}, g.DutchPiles()[0][0])
}

func TestBlitzToDutch_ReturnsError_WhenPlayingNonOneToEmptyPile(t *testing.T) {
	g := testutil.GameSetupNoError(2, t)
	player := game.Player(0)
	g.SetBlitzPile(player, []game.Card{{Number: 2, Color: 0, Player: player}})

	err := sut.BlitzToDutch(g, player, 0)

	assert.Error(t, err)
	assert.Equal(t, errors.BadAction(), err)
}

func TestBlitzToDutch_ReturnsError_WhenCardIsNotNextConsecutiveNumber(t *testing.T) {
	g := testutil.GameSetupNoError(2, t)
	player := game.Player(0)
	dutchPiles := g.DutchPiles()
	dutchPiles[0] = []game.Card{{Number: 1, Color: 0, Player: player}}
	g.SetBlitzPile(player, []game.Card{{Number: 3, Color: 0, Player: player}})

	err := sut.BlitzToDutch(g, player, 0)

	assert.Error(t, err)
	assert.Equal(t, errors.BadAction(), err)
}

func TestBlitzToDutch_ReturnsError_WhenCardColorDoesNotMatchPile(t *testing.T) {
	g := testutil.GameSetupNoError(2, t)
	player := game.Player(0)
	dutchPiles := g.DutchPiles()
	dutchPiles[0] = []game.Card{{Number: 1, Color: 0, Player: player}}
	g.SetBlitzPile(player, []game.Card{{Number: 2, Color: 1, Player: player}})

	err := sut.BlitzToDutch(g, player, 0)

	assert.Error(t, err)
	assert.Equal(t, errors.BadAction(), err)
}

func TestWoodPileToDutch_MovesTopWoodCardToDutchPile(t *testing.T) {
	g := testutil.GameSetupNoError(2, t)
	player := game.Player(0)
	g.SetWoodPile(player, []game.Card{{Number: 1, Color: 0, Player: player}})

	err := sut.WoodPileToDutch(g, player, 0)

	require.NoError(t, err)
	assert.Len(t, g.DutchPiles()[0], 1)
	assert.Len(t, g.WoodPile(player), 0)
	assert.Equal(t, game.Card{Number: 1, Color: 0, Player: player}, g.DutchPiles()[0][0])
}

func TestPostToDutch_MovesTopPostCardToDutchPile(t *testing.T) {
	g := testutil.GameSetupNoError(2, t)
	player := game.Player(0)
	postPiles := g.PostPiles(player)
	postPiles[0] = []game.Card{{Number: 1, Color: 0, Player: player}}
	g.SetPostPiles(player, postPiles)

	err := sut.PostToDutch(g, player, 0, 0)

	require.NoError(t, err)
	assert.Len(t, g.DutchPiles()[0], 1)
	assert.Len(t, g.PostPiles(player)[0], 0)
	assert.Equal(t, game.Card{Number: 1, Color: 0, Player: player}, g.DutchPiles()[0][0])
}

func TestWoodPileToDutch_ReturnsError_WhenPlayingNonOneToEmptyPile(t *testing.T) {
	g := testutil.GameSetupNoError(2, t)
	player := game.Player(0)
	g.SetWoodPile(player, []game.Card{{Number: 2, Color: 0, Player: player}})

	err := sut.WoodPileToDutch(g, player, 0)

	assert.Error(t, err)
	assert.Equal(t, errors.BadAction(), err)
}

func TestWoodPileToDutch_ReturnsError_WhenCardIsNotNextConsecutiveNumber(t *testing.T) {
	g := testutil.GameSetupNoError(2, t)
	player := game.Player(0)
	dutchPiles := g.DutchPiles()
	dutchPiles[0] = []game.Card{{Number: 1, Color: 0, Player: player}}
	g.SetWoodPile(player, []game.Card{{Number: 3, Color: 0, Player: player}})

	err := sut.WoodPileToDutch(g, player, 0)

	assert.Error(t, err)
}

func TestWoodPileToDutch_ReturnsError_WhenCardColorDoesNotMatchPile(t *testing.T) {
	g := testutil.GameSetupNoError(2, t)
	player := game.Player(0)
	dutchPiles := g.DutchPiles()
	dutchPiles[0] = []game.Card{{Number: 1, Color: 0, Player: player}}
	g.SetWoodPile(player, []game.Card{{Number: 2, Color: 1, Player: player}})

	err := sut.WoodPileToDutch(g, player, 0)

	assert.Error(t, err)
}

func TestPostToDutch_ReturnsError_WhenPlayingNonOneToEmptyPile(t *testing.T) {
	g := testutil.GameSetupNoError(2, t)
	player := game.Player(0)
	postPiles := g.PostPiles(player)
	postPiles[0] = []game.Card{{Number: 2, Color: 0, Player: player}}
	g.SetPostPiles(player, postPiles)

	err := sut.PostToDutch(g, player, 0, 0)

	assert.Error(t, err)
}

func TestPostToDutch_ReturnsError_WhenCardIsNotNextConsecutiveNumber(t *testing.T) {
	g := testutil.GameSetupNoError(2, t)
	player := game.Player(0)
	dutchPiles := g.DutchPiles()
	dutchPiles[0] = []game.Card{{Number: 1, Color: 0, Player: player}}
	postPiles := g.PostPiles(player)
	postPiles[0] = []game.Card{{Number: 3, Color: 0, Player: player}}
	g.SetPostPiles(player, postPiles)

	err := sut.PostToDutch(g, player, 0, 0)

	assert.Error(t, err)
}

func TestPostToDutch_ReturnsError_WhenCardColorDoesNotMatchPile(t *testing.T) {
	g := testutil.GameSetupNoError(2, t)
	player := game.Player(0)
	dutchPiles := g.DutchPiles()
	dutchPiles[0] = []game.Card{{Number: 1, Color: 0, Player: player}}
	postPiles := g.PostPiles(player)
	postPiles[0] = []game.Card{{Number: 2, Color: 1, Player: player}}
	g.SetPostPiles(player, postPiles)

	err := sut.PostToDutch(g, player, 0, 0)

	assert.Error(t, err)
}

func TestBlitzToPost_MovesTopBlitzCardToPostPile(t *testing.T) {
	g := testutil.GameSetupNoError(2, t)
	player := game.Player(0)
	g.SetBlitzPile(player, []game.Card{{Number: 5, Color: 0, Player: player}})
	postPiles := g.PostPiles(player)
	postPiles[0] = []game.Card{}
	g.SetPostPiles(player, postPiles)

	err := sut.BlitzToPost(g, player, 0)

	require.NoError(t, err)
	assert.Len(t, g.PostPiles(player)[0], 1)
	assert.Len(t, g.BlitzPile(player), 0)
	assert.Equal(t, game.Card{Number: 5, Color: 0, Player: player}, g.PostPiles(player)[0][0])
}

func TestBlitzToPost_ReturnsError_WhenCardIsNotNextDescendingNumber(t *testing.T) {
	g := testutil.GameSetupNoError(2, t)
	player := game.Player(0)
	postPiles := g.PostPiles(player)
	postPiles[0] = []game.Card{{Number: 5, Color: 0, Player: player}}
	g.SetPostPiles(player, postPiles)
	g.SetBlitzPile(player, []game.Card{{Number: 3, Color: 0, Player: player}})

	err := sut.BlitzToPost(g, player, 0)

	assert.Error(t, err)
	assert.Equal(t, errors.BadAction(), err)
}

func TestBlitzToPost_ReturnsError_WhenColorDoesNotAlternate(t *testing.T) {
	g := testutil.GameSetupNoError(2, t)
	player := game.Player(0)
	postPiles := g.PostPiles(player)
	postPiles[0] = []game.Card{{Number: 4, Color: 0, Player: player}}
	g.SetPostPiles(player, postPiles)
	g.SetBlitzPile(player, []game.Card{{Number: 5, Color: 2, Player: player}})

	err := sut.BlitzToPost(g, player, 0)

	assert.Error(t, err)
	assert.Equal(t, errors.BadAction(), err)
}

func TestWoodPileToPost_MovesTopWoodCardToPostPile(t *testing.T) {
	g := testutil.GameSetupNoError(2, t)
	player := game.Player(0)
	g.SetWoodPile(player, []game.Card{{Number: 5, Color: 0, Player: player}})
	postPiles := g.PostPiles(player)
	postPiles[0] = []game.Card{}
	g.SetPostPiles(player, postPiles)

	err := sut.WoodPileToPost(g, player, 0)

	require.NoError(t, err)
	assert.Len(t, g.PostPiles(player)[0], 1)
	assert.Len(t, g.WoodPile(player), 0)
	assert.Equal(t, game.Card{Number: 5, Color: 0, Player: player}, g.PostPiles(player)[0][0])
}

func TestWoodPileToPost_ReturnsError_WhenCardIsNotNextDescendingNumber(t *testing.T) {
	g := testutil.GameSetupNoError(2, t)
	player := game.Player(0)
	postPiles := g.PostPiles(player)
	postPiles[0] = []game.Card{{Number: 5, Color: 0, Player: player}}
	g.SetPostPiles(player, postPiles)
	g.SetWoodPile(player, []game.Card{{Number: 3, Color: 1, Player: player}})

	err := sut.WoodPileToPost(g, player, 0)

	assert.Error(t, err)
}

func TestWoodPileToPost_ReturnsError_WhenColorDoesNotAlternate(t *testing.T) {
	g := testutil.GameSetupNoError(2, t)
	player := game.Player(0)
	postPiles := g.PostPiles(player)
	postPiles[0] = []game.Card{{Number: 5, Color: 0, Player: player}}
	g.SetPostPiles(player, postPiles)
	g.SetWoodPile(player, []game.Card{{Number: 4, Color: 2, Player: player}})

	err := sut.WoodPileToPost(g, player, 0)

	assert.Error(t, err)
}

func TestPostToPost_MovesTopCardFromOnePostPileToAnother(t *testing.T) {
	g := testutil.GameSetupNoError(2, t)
	player := game.Player(0)
	postPiles := g.PostPiles(player)
	postPiles[0] = []game.Card{{Number: 5, Color: 0, Player: player}}
	postPiles[1] = []game.Card{}
	g.SetPostPiles(player, postPiles)

	err := sut.PostToPost(g, player, 0, 1)

	require.NoError(t, err)
	assert.Len(t, g.PostPiles(player)[0], 0)
	assert.Len(t, g.PostPiles(player)[1], 1)
	assert.Equal(t, 5, g.PostPiles(player)[1][0].Number)
}

func TestPostToPost_ReturnsError_WhenCardIsNotNextDescendingNumber(t *testing.T) {
	g := testutil.GameSetupNoError(2, t)
	player := game.Player(0)
	postPiles := g.PostPiles(player)
	postPiles[1] = []game.Card{{Number: 5, Color: 0, Player: player}}
	postPiles[0] = []game.Card{{Number: 3, Color: 1, Player: player}}
	g.SetPostPiles(player, postPiles)

	err := sut.PostToPost(g, player, 0, 1)

	assert.Error(t, err)
}

func TestPostToPost_ReturnsError_WhenColorDoesNotAlternate(t *testing.T) {
	g := testutil.GameSetupNoError(2, t)
	player := game.Player(0)
	postPiles := g.PostPiles(player)
	postPiles[1] = []game.Card{{Number: 5, Color: 0, Player: player}}
	postPiles[0] = []game.Card{{Number: 4, Color: 2, Player: player}}
	g.SetPostPiles(player, postPiles)

	err := sut.PostToPost(g, player, 0, 1)

	assert.Error(t, err)
}

func TestResetWoodPile_RestoresHandToInitialState_AfterEmptyingHand(t *testing.T) {
	g := testutil.GameSetupNoError(2, t)
	player := game.Player(0)
	initialHand := g.Hand(player)

	testutil.EmptyHand(g, player)

	err := sut.ResetWoodPile(g, player)

	require.NoError(t, err)
	assert.Equal(t, initialHand, g.Hand(player))
	assert.Empty(t, g.WoodPile(player))
}

func TestCycleHand_MovesFirstCardToEnd(t *testing.T) {
	g := testutil.GameSetupNoError(2, t)
	player := game.Player(0)
	g.SetCyclable(player, true)
	firstCard := g.Hand(player)[0]
	handLen := len(g.Hand(player))

	err := sut.CycleHand(g, player)

	require.NoError(t, err)
	assert.Len(t, g.Hand(player), handLen)
	assert.Equal(t, firstCard, g.Hand(player)[handLen-1])
}

func TestCycleHand_ReturnsError_WhenPlayerNotCyclable(t *testing.T) {
	g := testutil.GameSetupNoError(2, t)
	player := game.Player(0)

	err := sut.CycleHand(g, player)

	assert.Error(t, err)
	// assert.Equal(t, errors.BadAction(), err)
}

func TestResetWoodPile_MakesPlayerCyclable(t *testing.T) {
	g := testutil.GameSetupNoError(2, t)
	player := game.Player(0)

	testutil.EmptyHand(g, player)

	err := sut.ResetWoodPile(g, player)

	require.NoError(t, err)
	assert.True(t, g.IsCyclable(player))
}

func TestCycleHand_ReturnsError_AfterPlayingCardSinceReset(t *testing.T) {
	g := testutil.GameSetupNoError(2, t)
	player := game.Player(0)

	err := testutil.EmptyHandAndResetWoodPile(g, player)
	require.NoError(t, err)

	g.SetBlitzPile(player, []game.Card{{Number: 1, Color: 0, Player: player}})
	err = sut.BlitzToDutch(g, player, 0)
	require.NoError(t, err)

	err = sut.CycleHand(g, player)

	assert.Error(t, err)
}

func TestCycleHand_Succeeds_AfterResettingTwice(t *testing.T) {
	g := testutil.GameSetupNoError(2, t)
	player := game.Player(0)

	err := testutil.EmptyHandAndResetWoodPile(g, player)
	require.NoError(t, err)

	g.SetBlitzPile(player, []game.Card{{Number: 1, Color: 0, Player: player}})
	err = sut.BlitzToDutch(g, player, 0)
	require.NoError(t, err)

	err = testutil.EmptyHandAndResetWoodPile(g, player)
	require.NoError(t, err)

	err = sut.CycleHand(g, player)

	assert.NoError(t, err)
}

func TestCycleHand_ReturnsError_AfterPostToDutch(t *testing.T) {
	g := testutil.GameSetupNoError(2, t)
	player := game.Player(0)

	err := testutil.EmptyHandAndResetWoodPile(g, player)
	require.NoError(t, err)

	postPiles := g.PostPiles(player)
	postPiles[0] = []game.Card{{Number: 1, Color: 0, Player: player}}
	g.SetPostPiles(player, postPiles)
	err = sut.PostToDutch(g, player, 0, 0)
	require.NoError(t, err)

	err = sut.CycleHand(g, player)

	assert.Error(t, err)
}

func TestCycleHand_ReturnsError_AfterWoodPileToDutch(t *testing.T) {
	g := testutil.GameSetupNoError(2, t)
	player := game.Player(0)

	err := testutil.EmptyHandAndResetWoodPile(g, player)
	require.NoError(t, err)

	g.SetWoodPile(player, []game.Card{{Number: 1, Color: 0, Player: player}})
	err = sut.WoodPileToDutch(g, player, 0)
	require.NoError(t, err)

	err = sut.CycleHand(g, player)

	assert.Error(t, err)
}

func TestCycleHand_ReturnsError_AfterBlitzToPost(t *testing.T) {
	g := testutil.GameSetupNoError(2, t)
	player := game.Player(0)

	err := testutil.EmptyHandAndResetWoodPile(g, player)
	require.NoError(t, err)

	g.SetBlitzPile(player, []game.Card{{Number: 5, Color: 0, Player: player}})
	postPiles := g.PostPiles(player)
	postPiles[0] = []game.Card{}
	g.SetPostPiles(player, postPiles)
	err = sut.BlitzToPost(g, player, 0)
	require.NoError(t, err)

	err = sut.CycleHand(g, player)

	assert.Error(t, err)
}

func TestCycleHand_ReturnsError_AfterWoodPileToPost(t *testing.T) {
	g := testutil.GameSetupNoError(2, t)
	player := game.Player(0)

	err := testutil.EmptyHandAndResetWoodPile(g, player)
	require.NoError(t, err)

	g.SetWoodPile(player, []game.Card{{Number: 5, Color: 0, Player: player}})
	postPiles := g.PostPiles(player)
	postPiles[0] = []game.Card{}
	g.SetPostPiles(player, postPiles)
	err = sut.WoodPileToPost(g, player, 0)
	require.NoError(t, err)

	err = sut.CycleHand(g, player)

	assert.Error(t, err)
}

func TestCycleHand_ReturnsError_AfterPostToPost(t *testing.T) {
	g := testutil.GameSetupNoError(2, t)
	player := game.Player(0)

	err := testutil.EmptyHandAndResetWoodPile(g, player)
	require.NoError(t, err)

	postPiles := g.PostPiles(player)
	postPiles[0] = []game.Card{{Number: 5, Color: 0, Player: player}}
	postPiles[1] = []game.Card{}
	g.SetPostPiles(player, postPiles)
	err = sut.PostToPost(g, player, 0, 1)
	require.NoError(t, err)

	err = sut.CycleHand(g, player)

	assert.Error(t, err)
}

func TestHasWon_ReturnsTrue_WhenBlitzPileEmpty(t *testing.T) {
	g := testutil.GameSetupNoError(2, t)
	player := game.Player(0)
	g.SetBlitzPile(player, []game.Card{})

	result := sut.HasWon(g, player)

	assert.True(t, result)
}

func TestHasWon_ReturnsFalse_WhenBlitzPileNotEmpty(t *testing.T) {
	g := testutil.GameSetupNoError(2, t)
	player := game.Player(0)

	result := sut.HasWon(g, player)

	assert.False(t, result)
}

func TestBlitzToDutch_ReturnsError_WhenBlitzPileIsEmpty(t *testing.T) {
	g := testutil.GameSetupNoError(2, t)
	player := game.Player(0)
	g.SetBlitzPile(player, []game.Card{})

	err := sut.BlitzToDutch(g, player, 0)

	assert.Error(t, err)
}

func TestPostToDutch_ReturnsError_WhenPostPileIsEmpty(t *testing.T) {
	g := testutil.GameSetupNoError(2, t)
	player := game.Player(0)
	postPiles := g.PostPiles(player)
	postPiles[0] = []game.Card{}
	g.SetPostPiles(player, postPiles)

	err := sut.PostToDutch(g, player, 0, 0)

	assert.Error(t, err)
}

func TestWoodPileToDutch_ReturnsError_WhenWoodPileIsEmpty(t *testing.T) {
	g := testutil.GameSetupNoError(2, t)
	player := game.Player(0)

	err := sut.WoodPileToDutch(g, player, 0)

	assert.Error(t, err)
}

func TestBlitzToPost_ReturnsError_WhenBlitzPileIsEmpty(t *testing.T) {
	g := testutil.GameSetupNoError(2, t)
	player := game.Player(0)
	g.SetBlitzPile(player, []game.Card{})
	postPiles := g.PostPiles(player)
	postPiles[0] = []game.Card{}
	g.SetPostPiles(player, postPiles)

	err := sut.BlitzToPost(g, player, 0)

	assert.Error(t, err)
}

func TestWoodPileToPost_ReturnsError_WhenWoodPileIsEmpty(t *testing.T) {
	g := testutil.GameSetupNoError(2, t)
	player := game.Player(0)

	err := sut.WoodPileToPost(g, player, 0)

	assert.Error(t, err)
}

func TestPostToPost_ReturnsError_WhenSourcePostPileIsEmpty(t *testing.T) {
	g := testutil.GameSetupNoError(2, t)
	player := game.Player(0)
	postPiles := g.PostPiles(player)
	postPiles[0] = []game.Card{}
	g.SetPostPiles(player, postPiles)

	err := sut.PostToPost(g, player, 0, 1)

	assert.Error(t, err)
}

func TestBlitzToDutch_TakesFromLastElement(t *testing.T) {
	g := testutil.GameSetupNoError(2, t)
	player := game.Player(0)

	g.SetBlitzPile(player, []game.Card{
		{Number: 5, Color: 0, Player: player},
		{Number: 1, Color: 0, Player: player},
	})

	err := sut.BlitzToDutch(g, player, 0)

	require.NoError(t, err)
	assert.Len(t, g.BlitzPile(player), 1)
	assert.Equal(t, 5, g.BlitzPile(player)[0].Number)
}

func TestBlitzToPost_TakesFromLastElement(t *testing.T) {
	g := testutil.GameSetupNoError(2, t)
	player := game.Player(0)

	g.SetBlitzPile(player, []game.Card{
		{Number: 3, Color: 0, Player: player},
		{Number: 7, Color: 0, Player: player},
	})

	postPiles := g.PostPiles(player)
	postPiles[0] = []game.Card{}
	g.SetPostPiles(player, postPiles)

	err := sut.BlitzToPost(g, player, 0)

	require.NoError(t, err)
	assert.Len(t, g.BlitzPile(player), 1)
	assert.Equal(t, 3, g.BlitzPile(player)[0].Number)
	assert.Equal(t, 7, g.PostPiles(player)[0][0].Number)
}

func TestWoodPileToDutch_TakesFromLastElement(t *testing.T) {
	g := testutil.GameSetupNoError(2, t)
	player := game.Player(0)

	g.SetWoodPile(player, []game.Card{
		{Number: 5, Color: 0, Player: player},
		{Number: 1, Color: 0, Player: player},
	})

	err := sut.WoodPileToDutch(g, player, 0)

	require.NoError(t, err)
	assert.Len(t, g.WoodPile(player), 1)
	assert.Equal(t, 5, g.WoodPile(player)[0].Number)
}

func TestWoodPileToPost_TakesFromLastElement(t *testing.T) {
	g := testutil.GameSetupNoError(2, t)
	player := game.Player(0)

	g.SetWoodPile(player, []game.Card{
		{Number: 3, Color: 0, Player: player},
		{Number: 7, Color: 0, Player: player},
	})

	postPiles := g.PostPiles(player)
	postPiles[0] = []game.Card{}
	g.SetPostPiles(player, postPiles)

	err := sut.WoodPileToPost(g, player, 0)

	require.NoError(t, err)
	assert.Len(t, g.WoodPile(player), 1)
	assert.Equal(t, 3, g.WoodPile(player)[0].Number)
	assert.Equal(t, 7, g.PostPiles(player)[0][0].Number)
}

func TestAddToWoodPile_LastCardDealtIsAtEnd(t *testing.T) {
	g := testutil.GameSetupNoError(2, t)
	player := game.Player(0)

	sut.AddToWoodPile(g, player)

	wood := g.WoodPile(player)
	assert.Len(t, wood, 3)

	assert.Equal(t, 8, wood[len(wood)-1].Number)
	assert.Equal(t, 6, wood[0].Number)
}
