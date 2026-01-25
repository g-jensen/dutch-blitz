package badplayercount

type BadPlayerCountError struct {
	PlayerCount    int
	MinPlayerCount int
	MaxPlayerCount int
}

func New(playerCount int, minPlayerCount int, maxPlayerCount int) BadPlayerCountError {
	return BadPlayerCountError{
		PlayerCount:    playerCount,
		MinPlayerCount: minPlayerCount,
		MaxPlayerCount: maxPlayerCount,
	}
}

func (e BadPlayerCountError) Error() string {
	return "invalid player count"
}
