package badaction

type BadActionError struct{}

func New() BadActionError {
	return BadActionError{}
}

func (e BadActionError) Error() string {
	return "bad action"
}
