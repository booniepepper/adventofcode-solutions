#!/usr/bin/env tclsh

set fp [open "input" r]
set input [read $fp]
close $fp
set lines [split $input "\n"]
set count [llength $lines]

# Solution 1

set n_increased 0

for {set i 0} {$i < $count - 1} {incr i} {
  set j [expr $i + 1]

  set a [lindex $lines $i]
  set b [lindex $lines $j]

  if {$a == "" || $b == ""} { continue }

  set n_increased [expr $n_increased + [expr $a < $b]]
}

puts "Measurements increased $n_increased times."

# Solution 2

set n_increased 0
set prev_sum MAX_INT

for {set i 0} {$i < $count - 1} {incr i} {
  set window [lmap w {0 1 2} {
    set n [lindex $lines [expr $i + $w]]
    expr {
      [expr [string length $n] == 0] ? [continue] : $n
    }
  }]

  if {[llength $window] < 3} {
    continue
  }

  set sum 0
  foreach n $window {
    set sum [expr $sum + $n]
  }

  if {$sum > $prev_sum} {
    incr n_increased
  }

  set prev_sum $sum
}

puts "Three-measurement sliding window sums increased $n_increased times."
