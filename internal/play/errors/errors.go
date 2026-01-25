package errors

type BadActionError struct{}

func BadAction() *BadActionError {
	return &BadActionError{}
}

func (e *BadActionError) Error() string {
	return "bad action"
}

type BadPlayerCountError struct {
	PlayerCount    int
	MinPlayerCount int
	MaxPlayerCount int
}

func BadPlayerCount(playerCount int, minPlayerCount int, maxPlayerCount int) *BadPlayerCountError {
	return &BadPlayerCountError{
		PlayerCount:    playerCount,
		MinPlayerCount: minPlayerCount,
		MaxPlayerCount: maxPlayerCount,
	}
}

func (e *BadPlayerCountError) Error() string {
	return "invalid player count"
}
