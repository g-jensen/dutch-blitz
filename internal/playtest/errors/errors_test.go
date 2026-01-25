package errors

import (
	sut "dutch_blitz/internal/play/errors"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestBadAction_Error(t *testing.T) {
	expected_str := "bad action"
	assert.Equal(t, expected_str, sut.BadAction().Error())
}

func TestInvalidPlayerCount_Error(t *testing.T) {
	expected_str := "invalid player count"

	err := sut.BadPlayerCount(0, 1, 2)
	assert.Equal(t, expected_str, err.Error())
	assert.Equal(t, 0, err.PlayerCount)
	assert.Equal(t, 1, err.MinPlayerCount)
	assert.Equal(t, 2, err.MaxPlayerCount)

	err = sut.BadPlayerCount(1, 2, 3)
	assert.Equal(t, expected_str, err.Error())
	assert.Equal(t, 1, err.PlayerCount)
	assert.Equal(t, 2, err.MinPlayerCount)
	assert.Equal(t, 3, err.MaxPlayerCount)
}
