package badactiontest

import (
	sut "dutch_blitz/internal/play/errors/badaction"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestInvalidPlayerCountError_String(t *testing.T) {
	expected_str := "bad action"
	assert.Equal(t, expected_str, sut.New().Error())
}
