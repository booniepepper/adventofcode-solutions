#! /usr/bin/env tclsh

proc get_lines {filename} {
    set handle [open $filename r]
    set contents [read $handle]
    close $handle
    set lines [split $contents "\n"]
    return $lines
}

proc get_tiles {} {
    set lines [get_lines input]
    set tiles []
    set tile ""
    foreach line $lines {
        if {$line == ""} {
          lappend $tiles [$tile]
          set tile ""
        } else {
          lappend $tile $line
        }
    }
    return $tiles
}

puts [get_tiles]

