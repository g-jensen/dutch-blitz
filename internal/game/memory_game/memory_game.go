package memorygame

import (
	"dutch_blitz/internal/game"
)

type PlayerState struct {
	hand               []game.Card
	woodPile           []game.Card
	blitzPile          []game.Card
	postPiles          [][]game.Card
	hasPlayedThisReset bool
	isCyclable         bool
}

type MemoryGame struct {
	players      []game.Player
	playerStates []PlayerState
	dutchPiles   [][]game.Card
}

func New(playerCount int) *MemoryGame {
	g := &MemoryGame{}
	g.players = make([]game.Player, playerCount)
	for i := range g.players {
		g.players[i] = game.Player(i)
	}
	g.playerStates = make([]PlayerState, playerCount)
	return g
}

func (g *MemoryGame) Players() []game.Player {
	return g.players
}

func (g *MemoryGame) Hand(player game.Player) []game.Card {
	return g.playerStates[player].hand
}

func (g *MemoryGame) SetHand(player game.Player, newHand []game.Card) {
	g.playerStates[player].hand = newHand
}

func (g *MemoryGame) WoodPile(player game.Player) []game.Card {
	return g.playerStates[player].woodPile
}

func (g *MemoryGame) SetWoodPile(player game.Player, newWoodPile []game.Card) {
	g.playerStates[player].woodPile = newWoodPile
}

func (g *MemoryGame) BlitzPile(player game.Player) []game.Card {
	return g.playerStates[player].blitzPile
}

func (g *MemoryGame) SetBlitzPile(player game.Player, newBlitzPile []game.Card) {
	g.playerStates[player].blitzPile = newBlitzPile
}

func (g *MemoryGame) PostPiles(player game.Player) [][]game.Card {
	return g.playerStates[player].postPiles
}

func (g *MemoryGame) SetPostPiles(player game.Player, newPostPiles [][]game.Card) {
	g.playerStates[player].postPiles = newPostPiles
}

func (g *MemoryGame) DutchPiles() [][]game.Card {
	return g.dutchPiles
}

func (g *MemoryGame) SetDutchPiles(newDutchPiles [][]game.Card) {
	g.dutchPiles = newDutchPiles
}

func (g *MemoryGame) HasPlayedThisReset(player game.Player) bool {
	return g.playerStates[player].hasPlayedThisReset
}

func (g *MemoryGame) SetHasPlayedThisReset(player game.Player, hasPlayed bool) {
	g.playerStates[player].hasPlayedThisReset = hasPlayed
}

func (g *MemoryGame) IsCyclable(player game.Player) bool {
	return g.playerStates[player].isCyclable
}

func (g *MemoryGame) SetCyclable(player game.Player, isCyclable bool) {
	g.playerStates[player].isCyclable = isCyclable
}
