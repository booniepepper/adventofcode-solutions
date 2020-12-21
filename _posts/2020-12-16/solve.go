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
	myTicket := parseTicket(lines[NFields+2])
	tickets := parseTickets(lines[NFields+5:])

        invalidFields := findInvalidFields(headers, tickets)

        // Part 1 solution
        fmt.Println("Sum of invalid fields:", sum(invalidFields))

        validTickets := removeInvalids(headers, tickets)
        orderedHeaders := orderHeaders(headers, validTickets)
        departureFields := getDepartureFields(orderedHeaders)
        solution2 = product(selectByIndices(myTicket, departureFields))

        // Part 2 solution
        fmt.Println("Product of departure fields in my ticket:", solution2)
}

func getDepartureFields(headers []Header) []int {
    fields := make([]int, 0)
    for i, header := range headers {
        if strings.HasPrefix(header.Name, "departure") {
            fields = append(fields, i)
        }
    }
    return fields
}

func orderHeaders(unorderedHeaders []Header, tickets [][]int) []Header {
    possibleHeaders := make([][]Header, NFields)

    // Find all possible headers for a given field
    for i := 0; i < NFields; i++ {
        for _, header := range unorderedHeaders {
            valid := true
            for _, ticket := range tickets {
                contained := false
                for _, interval := range header.Intervals {
                    if interval.Contains(ticket[i]) {
                        contained = true
                    }
                }
                valid = valid && contained
            }
            if valid {
                possibleHeaders[i] = append(possibleHeaders[i], header)
            }
        }
    }

    // Filter down so there is only one header per field
    filtered := false
    for !filtered {
        filtered = true
        settleds := make([]bool, NFields)
        for i, headers := range possibleHeaders {
            settled := 1 == len(headers)
            settleds[i] = settled
            filtered = filtered && settled
            if settled {
                settledName := headers[0].Name
                for j, unfilteredHeaders := range possibleHeaders {
                    if j != i {
                        filteredHeaders := make([]Header, 0)
                        for _, unfilteredHeader := range unfilteredHeaders {
                            if unfilteredHeader.Name != settledName {
                                filteredHeaders = append(filteredHeaders, unfilteredHeader)
                            }
                        }
                        possibleHeaders[j] = filteredHeaders
                    }
                }
            }
        }
    }

    headers := make([]Header, NFields)
    for i, actualHeaders := range possibleHeaders {
        headers[i] = actualHeaders[0]
    }

    return headers
}

func findInvalidFields(headers []Header, tickets [][]int) []int {
    invalids := make([]int, 0)
    for _, ticket := range tickets {
       invalids = append(invalids, findInvalidField(headers, ticket)...)
    }
    return invalids
}

// Wow, I finally get the hubbub about why people want generics in Go.
// No wonder the std library feels lower level rather than higher level.
// (They can't implement a generic filter, map, reduce etc. without ugly reflection hijinks)
// https://blog.golang.org/generics-next-step
// Whoa, Phil Wadler is on the case! (Haskell monads and Java 5 Generics and more)
func removeInvalids(headers []Header, tickets [][]int) [][]int {
    valids := make([][]int, 0)
    for _, ticket := range tickets {
        if isValidTicket(headers, ticket) {
            valids = append(valids, ticket)
        }
    }
    return valids
}

func isValidTicket(headers []Header, ticket []int) bool {
    return 0 == len(findInvalidField(headers, ticket)) && len(ticket) == NFields
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

func selectByIndices(ns []int, is []int) []int {
    selected := make([]int, 0)
    for _, i := range is {
        selected = append(selected, ns[i])
    }
    return selected
}

func product(ns []int) int {
    if len(ns) == 0 {
        return 0
    }
    product := 1
    for _, n := range ns {
        product *= n
    }
    return product
}

func sum(ns []int) int {
    sum := 0
    for _, n := range ns {
        sum += n
    }
    return sum
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

