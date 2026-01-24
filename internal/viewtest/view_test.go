package viewtest

import (
	"dutch_blitz/internal/game"
	"dutch_blitz/internal/game/memorygame"
	"dutch_blitz/internal/play"
	"dutch_blitz/internal/testutil"
	sut "dutch_blitz/internal/view"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestFromGame_ReturnsPlayerStatePerPlayer(t *testing.T) {
	g := testutil.GameSetupNoError(2, t)

	v := sut.FromGame(g)

	assert.Len(t, v.PlayerStates, 2)
	assert.Len(t, v.DutchPiles, 8)
}

func TestFromGame_PopulatesBlitzFields(t *testing.T) {
	g := testutil.GameSetupNoError(2, t)

	v := sut.FromGame(g)

	assert.Equal(t, 10, v.PlayerStates[0].BlitzCount)
	require.NotNil(t, v.PlayerStates[0].TopOfBlitz)
	expectedTopOfBlitz := game.Card{Number: 5, Color: 1, Player: 0}
	assert.Equal(t, expectedTopOfBlitz, *v.PlayerStates[0].TopOfBlitz)
}

func TestFromGame_PopulatesWoodPileFields(t *testing.T) {
	g := testutil.GameSetupNoError(2, t)
	play.AddToWoodPile(g, game.Player(0))

	v := sut.FromGame(g)

	assert.Equal(t, 3, v.PlayerStates[0].WoodCount)
	require.NotNil(t, v.PlayerStates[0].TopOfWood)
	expectedTopOfWood := game.Card{Number: 8, Color: 1, Player: 0}
	assert.Equal(t, expectedTopOfWood, *v.PlayerStates[0].TopOfWood)
}

func TestFromGame_PopulatesPostPiles(t *testing.T) {
	g := testutil.GameSetupNoError(2, t)

	v := sut.FromGame(g)

	require.Len(t, v.PlayerStates[0].PostPiles, 5)
	require.Len(t, v.PlayerStates[0].PostPiles[0], 1)
	expectedCard := game.Card{Number: 1, Color: 0, Player: 0}
	assert.Equal(t, expectedCard, v.PlayerStates[0].PostPiles[0][0])
}

func TestSetup_ReturnsViewOnSuccess(t *testing.T) {
	g := memorygame.New(2)

	v, err := sut.Setup(g, 2, testutil.Identity)

	require.NoError(t, err)
	assert.Len(t, v.PlayerStates, 2)
	assert.Len(t, v.DutchPiles, 8)
}

func TestSetup_ReturnsErrorOnFailure(t *testing.T) {
	g := memorygame.New(0)

	v, err := sut.Setup(g, 0, testutil.Identity)

	assert.Error(t, err)
	assert.Equal(t, sut.View{}, v)
}

func TestAddToWoodPile_ReturnsViewWithUpdatedWoodPile(t *testing.T) {
	g := testutil.GameSetupNoError(2, t)
	player := game.Player(0)

	v := sut.AddToWoodPile(g, player)

	assert.Equal(t, 3, v.PlayerStates[0].WoodCount)
}

func TestResetWoodPile_ReturnsViewOnSuccess(t *testing.T) {
	g := testutil.GameSetupNoError(2, t)
	player := game.Player(0)

	testutil.EmptyHand(g, player)

	v, err := sut.ResetWoodPile(g, player)

	require.NoError(t, err)
	assert.Equal(t, 0, v.PlayerStates[0].WoodCount)
}

func TestResetWoodPile_ReturnsErrorWhenHandNotEmpty(t *testing.T) {
	g := testutil.GameSetupNoError(2, t)
	player := game.Player(0)

	v, err := sut.ResetWoodPile(g, player)

	assert.Error(t, err)
	assert.Equal(t, sut.View{}, v)
}

func TestCycleHand_ReturnsViewOnSuccess(t *testing.T) {
	g := testutil.GameSetupNoError(2, t)
	player := game.Player(0)

	testutil.EmptyHandAndResetWoodPile(g, player)

	v, err := sut.CycleHand(g, player)

	require.NoError(t, err)
	assert.Len(t, v.PlayerStates, 2)
}

func TestCycleHand_ReturnsErrorWhenNotCyclable(t *testing.T) {
	g := testutil.GameSetupNoError(2, t)
	player := game.Player(0)

	v, err := sut.CycleHand(g, player)

	assert.Error(t, err)
	assert.Equal(t, sut.View{}, v)
}

func TestBlitzToDutch_ReturnsViewOnSuccess(t *testing.T) {
	g := testutil.GameSetupNoError(2, t)
	player := game.Player(0)
	g.SetBlitzPile(player, []game.Card{{Number: 1, Color: 0, Player: player}})

	v, err := sut.BlitzToDutch(g, player, 0)

	require.NoError(t, err)
	assert.Equal(t, 0, v.PlayerStates[0].BlitzCount)
	assert.Len(t, v.DutchPiles[0], 1)
}

func TestBlitzToDutch_ReturnsErrorWhenBlitzEmpty(t *testing.T) {
	g := testutil.GameSetupNoError(2, t)
	player := game.Player(0)
	g.SetBlitzPile(player, []game.Card{})

	v, err := sut.BlitzToDutch(g, player, 0)

	assert.Error(t, err)
	assert.Equal(t, sut.View{}, v)
}

func TestBlitzToPost_ReturnsViewOnSuccess(t *testing.T) {
	g := testutil.GameSetupNoError(2, t)
	player := game.Player(0)
	g.SetBlitzPile(player, []game.Card{{Number: 5, Color: 0, Player: player}})
	postPiles := g.PostPiles(player)
	postPiles[0] = []game.Card{}
	g.SetPostPiles(player, postPiles)

	v, err := sut.BlitzToPost(g, player, 0)

	require.NoError(t, err)
	assert.Equal(t, 0, v.PlayerStates[0].BlitzCount)
}

func TestBlitzToPost_ReturnsErrorWhenBlitzEmpty(t *testing.T) {
	g := testutil.GameSetupNoError(2, t)
	player := game.Player(0)
	g.SetBlitzPile(player, []game.Card{})

	v, err := sut.BlitzToPost(g, player, 0)

	assert.Error(t, err)
	assert.Equal(t, sut.View{}, v)
}

func TestPostToDutch_ReturnsViewOnSuccess(t *testing.T) {
	g := testutil.GameSetupNoError(2, t)
	player := game.Player(0)
	postPiles := g.PostPiles(player)
	postPiles[0] = []game.Card{{Number: 1, Color: 0, Player: player}}
	g.SetPostPiles(player, postPiles)

	v, err := sut.PostToDutch(g, player, 0, 0)

	require.NoError(t, err)
	assert.Len(t, v.PlayerStates[0].PostPiles[0], 0)
	assert.Len(t, v.DutchPiles[0], 1)
}

func TestPostToDutch_ReturnsErrorWhenPostEmpty(t *testing.T) {
	g := testutil.GameSetupNoError(2, t)
	player := game.Player(0)
	postPiles := g.PostPiles(player)
	postPiles[0] = []game.Card{}
	g.SetPostPiles(player, postPiles)

	v, err := sut.PostToDutch(g, player, 0, 0)

	assert.Error(t, err)
	assert.Equal(t, sut.View{}, v)
}

func TestPostToPost_ReturnsViewOnSuccess(t *testing.T) {
	g := testutil.GameSetupNoError(2, t)
	player := game.Player(0)
	postPiles := g.PostPiles(player)
	postPiles[0] = []game.Card{{Number: 5, Color: 0, Player: player}}
	postPiles[1] = []game.Card{}
	g.SetPostPiles(player, postPiles)

	v, err := sut.PostToPost(g, player, 0, 1)

	require.NoError(t, err)
	assert.Len(t, v.PlayerStates[0].PostPiles[0], 0)
	assert.Len(t, v.PlayerStates[0].PostPiles[1], 1)
}

func TestPostToPost_ReturnsErrorWhenSourceEmpty(t *testing.T) {
	g := testutil.GameSetupNoError(2, t)
	player := game.Player(0)
	postPiles := g.PostPiles(player)
	postPiles[0] = []game.Card{}
	g.SetPostPiles(player, postPiles)

	v, err := sut.PostToPost(g, player, 0, 1)

	assert.Error(t, err)
	assert.Equal(t, sut.View{}, v)
}

func TestWoodPileToDutch_ReturnsViewOnSuccess(t *testing.T) {
	g := testutil.GameSetupNoError(2, t)
	player := game.Player(0)
	g.SetWoodPile(player, []game.Card{{Number: 1, Color: 0, Player: player}})

	v, err := sut.WoodPileToDutch(g, player, 0)

	require.NoError(t, err)
	assert.Equal(t, 0, v.PlayerStates[0].WoodCount)
	assert.Len(t, v.DutchPiles[0], 1)
}

func TestWoodPileToDutch_ReturnsErrorWhenWoodEmpty(t *testing.T) {
	g := testutil.GameSetupNoError(2, t)
	player := game.Player(0)
	g.SetWoodPile(player, []game.Card{})

	v, err := sut.WoodPileToDutch(g, player, 0)

	assert.Error(t, err)
	assert.Equal(t, sut.View{}, v)
}

func TestWoodPileToPost_ReturnsViewOnSuccess(t *testing.T) {
	g := testutil.GameSetupNoError(2, t)
	player := game.Player(0)
	g.SetWoodPile(player, []game.Card{{Number: 5, Color: 0, Player: player}})
	postPiles := g.PostPiles(player)
	postPiles[0] = []game.Card{}
	g.SetPostPiles(player, postPiles)

	v, err := sut.WoodPileToPost(g, player, 0)

	require.NoError(t, err)
	assert.Equal(t, 0, v.PlayerStates[0].WoodCount)
	assert.Len(t, v.PlayerStates[0].PostPiles[0], 1)
}

func TestWoodPileToPost_ReturnsErrorWhenWoodEmpty(t *testing.T) {
	g := testutil.GameSetupNoError(2, t)
	player := game.Player(0)
	g.SetWoodPile(player, []game.Card{})

	v, err := sut.WoodPileToPost(g, player, 0)

	assert.Error(t, err)
	assert.Equal(t, sut.View{}, v)
}
