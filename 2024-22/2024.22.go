package main

import (
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

func cycle(n int) int {
	n = n ^ (n<<6)%16777216
	n = n ^ (n>>5)%16777216
	n = n ^ (n<<11)%16777216
	return n
}

func calcSum2000Cycles(input string) int {
	lines := strings.Split(input, "\r\n")
	sum := 0
	for _, line := range lines {
		n, _ := strconv.Atoi(line)
		for i := 0; i < 2000; i++ {
			n = cycle(n)
		}
		sum += n
	}
	return sum
}

func findBestSequence(input string) string {
	lines := strings.Split(input, "\r\n")
	sequences := make(map[string]int)

	for _, line := range lines {
		n, _ := strconv.Atoi(line)
		lastprice := 0
		last2price := 0

		lastpricechange := 0
		last2pricechange := 0
		last3pricechange := 0
		last4pricechange := 0

		first_occurrence := make(map[string]int)

		for i := 0; i < 2000; i++ {
			n = cycle(n)

			last2price = lastprice
			lastprice = n % 10

			last4pricechange = last3pricechange
			last3pricechange = last2pricechange
			last2pricechange = lastpricechange
			lastpricechange = lastprice - last2price

			if i >= 3 {
				key := fmt.Sprintf("%v,%v,%v,%v", last4pricechange, last3pricechange, last2pricechange, lastpricechange)
				_, first_occurrence_found := first_occurrence[key]
				if !first_occurrence_found {
					first_occurrence[key] = lastprice
				}
			}
		}

		for k, v := range first_occurrence {
			sequences[k] = sequences[k] + v
		}
	}

	best_sequence_k := ""
	best_sequence_v := 0
	for k, v := range sequences {
		if v > best_sequence_v {
			best_sequence_k = k
			best_sequence_v = v
		}
	}

	return best_sequence_k
}

func main() {
	defer timeTrack(time.Now(), "2024.22")
	bytes, err := os.ReadFile("2024-22.txt")
	if err != nil {
		fmt.Printf("error reading file")
	}
	fmt.Printf("part1: %v\n", calcSum2000Cycles(string(bytes)))
	fmt.Printf("part1: %v\n", findBestSequence(string(bytes)))
}
