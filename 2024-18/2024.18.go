package main

import (
	"fmt"
	"math"
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

var bestPath map[pos_t]int
var grid [][]bool
var n_rows int
var n_cols int

func findPath1(p pos_t, steps int) int {
	if p.row == n_rows-1 && p.col == n_cols-1 {
		return steps
	}

	if p.row < 0 || p.row >= n_rows || p.col < 0 || p.col >= n_cols {
		return math.MaxInt64
	}

	if grid[p.row][p.col] {
		return math.MaxInt64
	}

	bestSteps, bestStepsFound := bestPath[p]
	if bestStepsFound && bestSteps <= steps+1 {
		return math.MaxInt64
	}

	bestPath[p] = steps + 1
	return (min(
		findPath1(pos_t{p.row + 1, p.col}, steps+1),
		findPath1(pos_t{p.row, p.col + 1}, steps+1),
		findPath1(pos_t{p.row - 1, p.col}, steps+1),
		findPath1(pos_t{p.row, p.col - 1}, steps+1)))
}

func findPath(input string, max_row int, max_col int, bytes int) int {
	grid = make([][]bool, max_row)
	for i := 0; i < max_row; i++ {
		grid[i] = make([]bool, max_col)
	}

	lines := strings.Split(input, "\r\n")

	for i := 0; i < bytes; i++ {
		line := strings.Split(lines[i], ",")
		row, _ := strconv.Atoi(line[0])
		col, _ := strconv.Atoi(line[1])
		grid[row][col] = true
	}

	bestPath = make(map[pos_t]int)
	n_rows = max_row
	n_cols = max_col

	return findPath1(pos_t{0, 0}, 0)
}

func findPath2(input string, max_row int, max_col int) string {
	lines := strings.Split(input, "\r\n")
	grid = make([][]bool, max_row)
	for i := 0; i < max_row; i++ {
		grid[i] = make([]bool, max_col)
	}

	for try := 0; try < len(lines); try++ {
		line := strings.Split(lines[try], ",")
		row, _ := strconv.Atoi(line[0])
		col, _ := strconv.Atoi(line[1])
		grid[row][col] = true

		bestPath = make(map[pos_t]int)
		n_rows = max_row
		n_cols = max_col

		if try > 1024 {
			fmt.Printf("try %v\n", try)
			if findPath1(pos_t{0, 0}, 0) == math.MaxInt64 {
				return lines[try]
			}
		}
	}

	return "none found"
}

func main() {
	bytes, err := os.ReadFile("2024-18.txt")
	if err != nil {
		fmt.Printf("error reading file")
	}
	fmt.Printf("findPath1: %v\n", findPath(string(bytes), 71, 71, 1024))
	fmt.Printf("findPath2: %v\n", findPath2(string(bytes), 71, 71))
}
