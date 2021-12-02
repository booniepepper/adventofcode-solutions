#!/usr/bin/env groovy

// Solution 1

def (distance, depth) =
  new File("input")
    .collect {
      def (dir, amount) = it.split()
      switch (dir) {
        case "forward": [amount.toInteger(), 0];  break
        case "down":    [0, amount.toInteger()];  break
        case "up":      [0, -amount.toInteger()]; break
      }
    }
    .inject([0, 0]) { acc, delta ->
      def (x, y) = acc
      def (dx, dy) = delta
      [x + dx, y + dy]
    }

println("Distance: $distance, Depth: $depth")
println("Product: ${distance * depth}")
println()

// Solution 2

def (distance2, depth2) =
  new File("input")
    .collect {
      def (dir, amt) = it.split()
      [dir, amt.toInteger()]
    }
    .inject([0, 0, 0]) { acc, delta ->
      def (x, y, aim) = acc
      def (dir, amt) = delta
      switch (dir) {
        case "forward": [x + amt, y + amt * aim, aim]; break
        case "down":    [x, y, aim + amt];             break
        case "up":      [x, y, aim - amt];             break
      }
    }

println("Distance: $distance2, Depth: $depth2")
println("Product: ${distance2 * depth2}")
