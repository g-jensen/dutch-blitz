package play

import (
	"dutch_blitz/internal/game"
	"errors"
)

func generateDeck(player game.Player) []game.Card {
	deck := make([]game.Card, 0, 40)
	for color := range 4 {
		for number := 1; number <= 10; number++ {
			deck = append(deck, game.Card{Number: number, Color: game.Color(color), Player: player})
		}
	}
	return deck
}

func validatePlayerCount(playerCount int) error {
	if playerCount < 2 {
		return errors.New("not enough players")
	}
	if playerCount > 4 {
		return errors.New("too many players")
	}
	return nil
}

func postPileCountForPlayers(playerCount int) int {
	if playerCount == 2 {
		return 5
	}
	return 3
}

func TopCard(pile []game.Card) game.Card {
	if len(pile) == 0 {
		return game.Card{}
	}
	return pile[len(pile)-1]
}

func cardsAreConsecutive(card1 game.Card, card2 game.Card) bool {
	return card1.Number+1 == card2.Number
}

func colorsMatch(card1 game.Card, card2 game.Card) bool {
	return card1.Color == card2.Color
}

func validateDutchMove(card game.Card, dutchPile []game.Card) error {
	if len(dutchPile) == 0 && card.Number != 1 {
		return errors.New("must play 1 to empty pile")
	}
	if len(dutchPile) == 0 {
		return nil
	}
	TopCard := TopCard(dutchPile)
	if !cardsAreConsecutive(TopCard, card) {
		return errors.New("card must be next consecutive number")
	}
	if !colorsMatch(card, TopCard) {
		return errors.New("card color must match pile")
	}
	return nil
}

func dealCardsToPlayer(g game.Game, player game.Player, deck []game.Card, postPileCount int) {
	postPiles := make([][]game.Card, postPileCount)
	for i := range postPileCount {
		postPiles[i] = []game.Card{deck[i]}
	}
	g.SetPostPiles(player, postPiles)
	g.SetBlitzPile(player, deck[postPileCount:postPileCount+10])
	g.SetHand(player, deck[postPileCount+10:])
}

func Setup(g game.Game, playerCount int, shuffleFn func([]game.Card) []game.Card) error {
	if err := validatePlayerCount(playerCount); err != nil {
		return err
	}
	g.SetDutchPiles(make([][]game.Card, playerCount*4))
	postPileCount := postPileCountForPlayers(playerCount)
	for i := range playerCount {
		player := game.Player(i)
		deck := shuffleFn(generateDeck(player))
		dealCardsToPlayer(g, player, deck, postPileCount)
	}
	return nil
}

func AddToWoodPile(g game.Game, player game.Player) {
	hand := g.Hand(player)
	count := min(3, len(hand))
	toMove := hand[:count]
	remaining := hand[count:]
	wood := g.WoodPile(player)
	newWood := append(wood, toMove...)
	g.SetWoodPile(player, newWood)
	g.SetHand(player, remaining)
}

func ResetWoodPile(g game.Game, player game.Player) error {
	if len(g.Hand(player)) > 0 {
		return errors.New("hand not empty")
	}
	wood := g.WoodPile(player)
	g.SetHand(player, wood)
	g.SetWoodPile(player, []game.Card{})
	g.SetCyclable(player, true)
	g.SetHasPlayedThisReset(player, false)
	return nil
}

func CycleHand(g game.Game, player game.Player) error {
	if !g.IsCyclable(player) {
		return errors.New("not cyclable")
	}
	if g.HasPlayedThisReset(player) {
		return errors.New("cannot cycle after playing card")
	}
	hand := g.Hand(player)
	newHand := append(hand[1:], hand[0])
	g.SetHand(player, newHand)
	return nil
}

func BlitzToDutch(g game.Game, player game.Player, dutchIndex int) error {
	blitz := g.BlitzPile(player)
	if len(blitz) == 0 {
		return errors.New("blitz pile is empty")
	}
	card := TopCard(blitz)
	dutch := g.DutchPiles()
	pile := dutch[dutchIndex]
	if err := validateDutchMove(card, pile); err != nil {
		return err
	}
	g.SetBlitzPile(player, blitz[:len(blitz)-1])
	dutch[dutchIndex] = append(dutch[dutchIndex], card)
	g.SetHasPlayedThisReset(player, true)
	return nil
}

func PostToDutch(g game.Game, player game.Player, postIndex int, dutchIndex int) error {
	postPiles := g.PostPiles(player)
	pile := postPiles[postIndex]
	if len(pile) == 0 {
		return errors.New("post pile is empty")
	}
	card := TopCard(pile)
	dutch := g.DutchPiles()
	dutchPile := dutch[dutchIndex]
	if err := validateDutchMove(card, dutchPile); err != nil {
		return err
	}
	postPiles[postIndex] = pile[:len(pile)-1]
	g.SetPostPiles(player, postPiles)
	dutch[dutchIndex] = append(dutch[dutchIndex], card)
	g.SetHasPlayedThisReset(player, true)
	return nil
}

func WoodPileToDutch(g game.Game, player game.Player, dutchIndex int) error {
	wood := g.WoodPile(player)
	if len(wood) == 0 {
		return errors.New("wood pile is empty")
	}
	card := TopCard(wood)
	dutch := g.DutchPiles()
	pile := dutch[dutchIndex]
	if err := validateDutchMove(card, pile); err != nil {
		return err
	}
	g.SetWoodPile(player, wood[:len(wood)-1])
	dutch[dutchIndex] = append(dutch[dutchIndex], card)
	g.SetHasPlayedThisReset(player, true)
	return nil
}

func colorsAreAlternating(card1 game.Card, card2 game.Card) bool {
	return card1.Color%2 == card2.Color%2
}

func validatePostMove(card game.Card, postPile []game.Card) error {
	if len(postPile) == 0 {
		return nil
	}
	TopCard := TopCard(postPile)
	if !cardsAreConsecutive(TopCard, card) {
		return errors.New("card must be next descending number")
	}
	if !colorsAreAlternating(card, TopCard) {
		return errors.New("colors must alternate")
	}
	return nil
}

func BlitzToPost(g game.Game, player game.Player, postIndex int) error {
	blitz := g.BlitzPile(player)
	if len(blitz) == 0 {
		return errors.New("blitz pile is empty")
	}
	card := TopCard(blitz)
	postPiles := g.PostPiles(player)
	pile := postPiles[postIndex]
	if err := validatePostMove(card, pile); err != nil {
		return err
	}
	g.SetBlitzPile(player, blitz[:len(blitz)-1])
	postPiles[postIndex] = append(postPiles[postIndex], card)
	g.SetPostPiles(player, postPiles)
	g.SetHasPlayedThisReset(player, true)
	return nil
}

func WoodPileToPost(g game.Game, player game.Player, postIndex int) error {
	wood := g.WoodPile(player)
	if len(wood) == 0 {
		return errors.New("wood pile is empty")
	}
	card := TopCard(wood)
	postPiles := g.PostPiles(player)
	pile := postPiles[postIndex]
	if err := validatePostMove(card, pile); err != nil {
		return err
	}
	g.SetWoodPile(player, wood[:len(wood)-1])
	postPiles[postIndex] = append(postPiles[postIndex], card)
	g.SetPostPiles(player, postPiles)
	g.SetHasPlayedThisReset(player, true)
	return nil
}

func PostToPost(g game.Game, player game.Player, fromIndex int, toIndex int) error {
	postPiles := g.PostPiles(player)
	fromPile := postPiles[fromIndex]
	if len(fromPile) == 0 {
		return errors.New("source post pile is empty")
	}
	card := TopCard(fromPile)
	toPile := postPiles[toIndex]
	if err := validatePostMove(card, toPile); err != nil {
		return err
	}
	postPiles[fromIndex] = fromPile[:len(fromPile)-1]
	postPiles[toIndex] = append(postPiles[toIndex], card)
	g.SetPostPiles(player, postPiles)
	g.SetHasPlayedThisReset(player, true)
	return nil
}

func HasWon(g game.Game, player game.Player) bool {
	return len(g.BlitzPile(player)) == 0
}
