#!/usr/bin/env gorun
package main

import (
	"fmt"
	"io/ioutil"
	"strconv"
	"strings"
)

const NFields = 20

type Interval struct {
	Start, End int
}

func (i Interval) Contains(n int) bool {
    return i.Start <= n && n <= i.End
}

type Header struct {
	Name   string
	Intervals []Interval
}

func main() {
	input, _ := ioutil.ReadFile("./input")
	lines := strings.Split(string(input), "\n")

	headers := parseHeaders(lines[0:NFields])
	//myTicket := parseTicket(lines[NFields+2])
	tickets := parseTickets(lines[NFields+5:])

        invalidFields := findInvalidFields(headers, tickets)

        fmt.Println("Sum of invalid fields:", sum(invalidFields))
}

func sum(ns []int) int {
    sum := 0
    for _, n := range ns {
        sum += n
    }
    return sum
}

func findInvalidFields(headers []Header, tickets [][]int) []int {
    invalids := make([]int, 0)
    for _, ticket := range tickets {
       invalids = append(invalids, findInvalidField(headers, ticket)...)
    }
    return invalids
}

func findInvalidField(headers []Header, ticket []int) []int {
    invalids := make([]int, 0)
    for _, num := range ticket {
        valid := false
        for _, header := range headers {
            for _, interval := range header.Intervals {
                valid = valid || interval.Contains(num)
            }
        }
        if !valid {
            invalids = append(invalids, num)
        }
    }
    return invalids
}

func parseHeaders(rawHeaders []string) []Header {
	headers := make([]Header, len(rawHeaders))
	for i, line := range rawHeaders {
		parts := strings.Split(line, ": ")
		name := parts[0]
		rawIntervals := strings.Split(parts[1], " or ")
		intervals := make([]Interval, len(rawIntervals))
		for j, rawInterval := range rawIntervals {
			parsedInterval := strings.Split(rawInterval, "-")
			start, _ := strconv.Atoi(parsedInterval[0])
			end, _ := strconv.Atoi(parsedInterval[1])
			intervals[j] = Interval{start, end}
		}
		headers[i] = Header{name, intervals}
	}
	return headers
}

func parseTickets(rawTickets []string) [][]int {
	tickets := make([][]int, len(rawTickets))
	for i, rawTicket := range rawTickets {
		tickets[i] = parseTicket(rawTicket)
	}
	return tickets
}

func parseTicket(rawTicket string) []int {
	if len(rawTicket) == 0 {
		return []int{}
	}
	rawNums := strings.Split(rawTicket, ",")
	nums := make([]int, len(rawNums))
	for i, rawNum := range rawNums {
		nums[i], _ = strconv.Atoi(rawNum)
	}
	return nums
}

