package game

type Player int
type Color int

type Card struct {
	Number int
	Color  Color
	Player Player
}

type Game interface {
	Players() []Player

	Hand(player Player) []Card
	SetHand(player Player, newHand []Card)

	WoodPile(player Player) []Card
	SetWoodPile(player Player, newWoodPile []Card)

	BlitzPile(player Player) []Card
	SetBlitzPile(player Player, newBlitzPile []Card)

	PostPiles(player Player) [][]Card
	SetPostPiles(player Player, newPostPiles [][]Card)

	DutchPiles() [][]Card
	SetDutchPiles(newDutchPiles [][]Card)

	HasPlayedThisReset(player Player) bool
	SetHasPlayedThisReset(player Player, value bool)

	IsCyclable(player Player) bool
	SetCyclable(player Player, value bool)
}
