package badplayercounttest

import (
	sut "dutch_blitz/internal/play/errors/badplayercount"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestInvalidPlayerCountError_String(t *testing.T) {
	expected_str := "invalid player count"

	err := sut.New(0, 1, 2)
	assert.Equal(t, expected_str, err.Error())
	assert.Equal(t, 0, err.PlayerCount)
	assert.Equal(t, 1, err.MinPlayerCount)
	assert.Equal(t, 2, err.MaxPlayerCount)

	err = sut.New(1, 2, 3)
	assert.Equal(t, expected_str, err.Error())
	assert.Equal(t, 1, err.PlayerCount)
	assert.Equal(t, 2, err.MinPlayerCount)
	assert.Equal(t, 3, err.MaxPlayerCount)
}
