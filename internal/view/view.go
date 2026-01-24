package view

import (
	"dutch_blitz/internal/game"
	"dutch_blitz/internal/play"
)

type PlayerState struct {
	TopOfBlitz *game.Card
	BlitzCount int
	TopOfWood  *game.Card
	WoodCount  int
	PostPiles  [][]game.Card
}

type View struct {
	PlayerStates []PlayerState
	DutchPiles   [][]game.Card
}

func topCardPtr(pile []game.Card) *game.Card {
	if len(pile) == 0 {
		return nil
	}
	card := play.TopCard(pile)
	return &card
}

func playerState(g game.Game, player game.Player) PlayerState {
	blitzPile := g.BlitzPile(player)
	woodPile := g.WoodPile(player)
	return PlayerState{
		BlitzCount: len(blitzPile),
		TopOfBlitz: topCardPtr(blitzPile),
		WoodCount:  len(woodPile),
		TopOfWood:  topCardPtr(woodPile),
		PostPiles:  g.PostPiles(player),
	}
}

func FromGame(g game.Game) View {
	playerStates := make([]PlayerState, len(g.Players()))
	for i, player := range g.Players() {
		playerStates[i] = playerState(g, player)
	}
	return View{
		PlayerStates: playerStates,
		DutchPiles:   g.DutchPiles(),
	}
}

func Setup(g game.Game, playerCount int, shuffleFn play.ShuffleFn) (View, error) {
	if err := play.Setup(g, playerCount, shuffleFn); err != nil {
		return View{}, err
	}
	return FromGame(g), nil
}

func AddToWoodPile(g game.Game, player game.Player) View {
	play.AddToWoodPile(g, player)
	return FromGame(g)
}

func ResetWoodPile(g game.Game, player game.Player) (View, error) {
	if err := play.ResetWoodPile(g, player); err != nil {
		return View{}, err
	}
	return FromGame(g), nil
}

func CycleHand(g game.Game, player game.Player) (View, error) {
	if err := play.CycleHand(g, player); err != nil {
		return View{}, err
	}
	return FromGame(g), nil
}

func BlitzToDutch(g game.Game, player game.Player, dutchIndex int) (View, error) {
	if err := play.BlitzToDutch(g, player, dutchIndex); err != nil {
		return View{}, err
	}
	return FromGame(g), nil
}

func BlitzToPost(g game.Game, player game.Player, postIndex int) (View, error) {
	if err := play.BlitzToPost(g, player, postIndex); err != nil {
		return View{}, err
	}
	return FromGame(g), nil
}

func PostToDutch(g game.Game, player game.Player, postIndex int, dutchIndex int) (View, error) {
	if err := play.PostToDutch(g, player, postIndex, dutchIndex); err != nil {
		return View{}, err
	}
	return FromGame(g), nil
}

func PostToPost(g game.Game, player game.Player, fromIndex int, toIndex int) (View, error) {
	if err := play.PostToPost(g, player, fromIndex, toIndex); err != nil {
		return View{}, err
	}
	return FromGame(g), nil
}

func WoodPileToDutch(g game.Game, player game.Player, dutchIndex int) (View, error) {
	if err := play.WoodPileToDutch(g, player, dutchIndex); err != nil {
		return View{}, err
	}
	return FromGame(g), nil
}

func WoodPileToPost(g game.Game, player game.Player, postIndex int) (View, error) {
	if err := play.WoodPileToPost(g, player, postIndex); err != nil {
		return View{}, err
	}
	return FromGame(g), nil
}
