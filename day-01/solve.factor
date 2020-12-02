#!/usr/bin/env factor

USING: combinators formatting fry io.files io.encodings.utf8
kernel math math.combinatorics math.parser prettyprint
sequences ;
IN: solution

: read-input ( path -- seq )
    utf8 file-lines
    [ string>number ] map
;

! Return the product of n members that match a given sum k.
: solve ( seq n k -- seq )
    '[ sum _ = ] filter-combinations first
;

: print-solution ( seq -- )
    [ "%[%d, %] : " printf ] [ product . ] bi
;

"./input" read-input
{ 2 3 4 } [ [ 2020 solve print-solution ] curry ] map cleave

