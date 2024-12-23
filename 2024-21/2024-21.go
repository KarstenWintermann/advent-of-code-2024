package main

import (
	"bytes"
	"fmt"
	"os"
	"strconv"
	"strings"
	"time"
)

func timeTrack(start time.Time, name string) {
	elapsed := time.Since(start)
	fmt.Printf("%s took %s\n", name, elapsed)
}

type pos_t struct {
	row, col int
}

func pos_numpad(key string) pos_t {
	switch key {
	case "0":
		return pos_t{4, 2}
	case "A":
		return pos_t{4, 3}
	case "1":
		return pos_t{3, 1}
	case "2":
		return pos_t{3, 2}
	case "3":
		return pos_t{3, 3}
	case "4":
		return pos_t{2, 1}
	case "5":
		return pos_t{2, 2}
	case "6":
		return pos_t{2, 3}
	case "7":
		return pos_t{1, 1}
	case "8":
		return pos_t{1, 2}
	case "9":
		return pos_t{1, 3}
	}
	return pos_t{0, 0}
}

func move_numpad1(a, b string, buffer *bytes.Buffer) {
	pos_a := pos_numpad(a)
	pos_b := pos_numpad(b)
	row := pos_a.row
	row_2 := pos_b.row
	col := pos_a.col
	col_2 := pos_b.col

	// avoid (4,1)
	if (row == 4 && col_2 == 1) || (col < col_2 && col != 1) {
		for row > row_2 {
			buffer.WriteRune('^')
			row--
		}
		for row < row_2 {
			buffer.WriteRune('v')
			row++
		}
	}
	for col < col_2 {
		buffer.WriteRune('>')
		col++
	}
	for col > col_2 {
		buffer.WriteRune('<')
		col--
	}
	for row < row_2 {
		buffer.WriteRune('v')
		row++
	}
	for row > row_2 {
		buffer.WriteRune('^')
		row--
	}
}

func move_numpad(s string) string {
	var buffer bytes.Buffer

	s = "A" + s
	for i := 0; i < len(s)-1; i++ {
		move_numpad1(s[i:i+1], s[i+1:i+2], &buffer)
		buffer.WriteRune('A')
	}
	return buffer.String()
}

func pos_keypad(key string) pos_t {
	switch key {
	case "^":
		return pos_t{1, 2}
	case "A":
		return pos_t{1, 3}
	case "<":
		return pos_t{2, 1}
	case "v":
		return pos_t{2, 2}
	case ">":
		return pos_t{2, 3}
	}
	return pos_t{0, 0}
}

func move_keypad1(a, b string, buffer *bytes.Buffer) {
	pos_a := pos_keypad(a)
	pos_b := pos_keypad(b)
	row := pos_a.row
	row_2 := pos_b.row
	col := pos_a.col
	col_2 := pos_b.col

	// avoid (1,1)
	if (row == 1 && col_2 == 1) || (col < col_2 && col != 1) {
		for row < row_2 {
			buffer.WriteRune('v')
			row++
		}
		for row > row_2 {
			buffer.WriteRune('^')
			row--
		}
	}
	for col < col_2 {
		buffer.WriteRune('>')
		col++
	}
	for col > col_2 {
		buffer.WriteRune('<')
		col--
	}
	for row < row_2 {
		buffer.WriteRune('v')
		row++
	}
	for row > row_2 {
		buffer.WriteRune('^')
		row--
	}
}

func move_keypad(s string) string {
	var buffer bytes.Buffer

	s = "A" + s
	for i := 0; i < len(s)-1; i++ {
		move_keypad1(s[i:i+1], s[i+1:i+2], &buffer)
		buffer.WriteRune('A')
	}

	return buffer.String()
}

func complexity1(input string) int {
	lines := strings.Split(input, "\r\n")
	sum := 0
	for _, line := range lines {
		value, _ := strconv.Atoi(line[0:3])
		sum += len(move_keypad(move_keypad(move_numpad(line)))) * value
	}
	return sum
}

func complexity2(input string) int {
	lines := strings.Split(input, "\r\n")
	sum := 0
	for _, line := range lines {
		fmt.Printf("%v\n", line)
		value, _ := strconv.Atoi(line[0:3])
		s := move_numpad(line)
		for i := 0; i < 26; i++ {
			s = move_keypad(s)
			fmt.Printf("%v %v\n", i, len(s))
		}
		sum += len(s) * value
	}
	return sum
}

func main() {
	defer timeTrack(time.Now(), "2024.21")
	bytes, err := os.ReadFile("2024-21test.txt")
	if err != nil {
		fmt.Printf("error reading file")
	}
	fmt.Printf("part1: %v\n", complexity1(string(bytes)))
	fmt.Printf("part2: %v\n", complexity2(string(bytes)))
}
