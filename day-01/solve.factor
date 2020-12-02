USING: formatting io.files io.encodings.utf8 kernel locals math
math.combinatorics math.parser prettyprint sequences ;
IN: solution

: read-input ( path -- seq )
    utf8 file-lines
    [ string>number ] map
;

! Return the product of n members that match a given sum k.
:: solve ( seq n k -- seq )
    seq n all-combinations
    [ sum k = ] find nip
;

: print-solution ( seq -- )
    [ "%[%d, %] : " printf ] [ product . ] bi
;

"./input" read-input
[ 2 2020 solve print-solution ]
[ 3 2020 solve print-solution ] 
[ 4 2020 solve print-solution ] tri

