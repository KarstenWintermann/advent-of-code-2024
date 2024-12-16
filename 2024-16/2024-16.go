package main

import (
	"fmt"
	"math"
	"os"
	"strings"
	"time"
)

func timeTrack(start time.Time, name string) {
	elapsed := time.Since(start)
	fmt.Printf("%s took %s\n", name, elapsed)
}

var lines []string

var bestSteps map[state_t]int
var bestTiles map[pos_t]bool

type pos_t struct {
	x, y int
}

type vector_t struct {
	x, y int
}

type state_t struct {
	heading vector_t
	pos     pos_t
}

var RIGHT = vector_t{1, 0}
var DOWN = vector_t{0, 1}
var LEFT = vector_t{-1, 0}
var UP = vector_t{0, -1}

func turnright(vec vector_t) vector_t {
	switch vec {
	case RIGHT:
		return DOWN
	case DOWN:
		return LEFT
	case LEFT:
		return UP
	case UP:
		return RIGHT
	}

	return vector_t{0, 0}
}

func turnleft(vec vector_t) vector_t {
	switch vec {
	case RIGHT:
		return UP
	case DOWN:
		return RIGHT
	case LEFT:
		return DOWN
	case UP:
		return LEFT
	}

	return vector_t{0, 0}
}

func move(pos pos_t, direction vector_t) pos_t {
	return pos_t{pos.x + direction.x, pos.y + direction.y}
}

func maze_at_pos(pos pos_t) rune {
	return rune(lines[pos.y][pos.x])
}

func findStart() pos_t {
	for y := 0; y < len(lines); y++ {
		for x := 0; x < len(lines[y]); x++ {
			if rune(lines[y][x]) == 'S' {
				return pos_t{x, y}
			}
		}
	}
	return pos_t{0, 0}
}

func findPath(steps int, pos pos_t, direction vector_t) int {
	newPos := move(pos, direction)

	if maze_at_pos(newPos) == 'E' {
		return steps + 1
	}

	if maze_at_pos(newPos) == '#' {
		return math.MaxInt
	}

	state := state_t{direction, newPos}
	existingSteps, foundSteps := bestSteps[state]

	if foundSteps && existingSteps <= steps {
		return math.MaxInt
	}

	bestSteps[state] = steps

	for maze_at_pos(move(newPos, direction)) == '.' &&
		maze_at_pos(move(newPos, turnleft(direction))) == '#' &&
		maze_at_pos(move(newPos, turnright(direction))) == '#' {
		steps += 1
		newPos = move(newPos, direction)
		state := state_t{direction, newPos}
		existingSteps, foundSteps := bestSteps[state]

		if foundSteps && existingSteps <= steps {
			return math.MaxInt
		}
		bestSteps[state] = steps
	}

	return min(
		findPath(steps+1, newPos, direction),
		findPath(steps+1001, newPos, turnleft(direction)),
		findPath(steps+1001, newPos, turnright(direction)))
}

func countBestTiles(steps int, pos pos_t, direction vector_t, bestLength int) int {
	newPos := move(pos, direction)

	if maze_at_pos(newPos) == 'E' {
		bestTiles[newPos] = true
		return steps + 1
	}

	if maze_at_pos(newPos) == '#' {
		return math.MaxInt
	}

	state := state_t{direction, newPos}
	existingSteps, foundSteps := bestSteps[state]

	if foundSteps && existingSteps < steps {
		return math.MaxInt
	}

	bestSteps[state] = steps

	straight := countBestTiles(steps+1, newPos, direction, bestLength-1)
	left := countBestTiles(steps+1001, newPos, turnleft(direction), bestLength-1001)
	right := countBestTiles(steps+1001, newPos, turnright(direction), bestLength-1001)

	if straight-bestLength == steps ||
		left-bestLength == steps ||
		right-bestLength == steps {
		bestTiles[newPos] = true
	}

	return (min(straight, left, right))
}

func findBestPath(input string) int {
	lines = strings.Split(input, "\r\n")
	bestSteps = make(map[state_t]int)

	start := findStart()

	direction := RIGHT

	ret := min(
		findPath(0, start, direction),
		findPath(1000, start, turnright(direction)),
		findPath(1000, start, turnleft(direction)))
	return ret
}

func countBest(input string, bestLength int) int {
	lines = strings.Split(input, "\r\n")
	bestSteps = make(map[state_t]int)
	bestTiles = make(map[pos_t]bool)

	start := findStart()

	bestTiles[start] = true

	direction := RIGHT

	countBestTiles(0, start, direction, bestLength)
	countBestTiles(1000, start, turnright(direction), bestLength-1000)
	countBestTiles(1000, start, turnleft(direction), bestLength-1000)

	return len(bestTiles)
}

func main() {
	defer timeTrack(time.Now(), "2024.16")
	bytes, err := os.ReadFile("2024-16.txt")
	if err != nil {
		fmt.Printf("error reading file")
	}
	bestLength := findBestPath(string(bytes))
	fmt.Printf("part1: %v\n", bestLength)
	fmt.Printf("part2: %v\n", countBest(string(bytes), bestLength))
}
