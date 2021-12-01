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

puts "Number increased: $n_increased"
