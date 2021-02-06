#! /usr/bin/env tclsh

proc get_tiles {filename} {
    set handle [open $filename r]
    set contents [read $handle]
    close $handle
    set tiles [split $contents "\n\n"]
    return $tiles
}

puts [lindex [get_tiles input] 0]

