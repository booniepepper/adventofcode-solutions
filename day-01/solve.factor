USING: io.files io.encodings.utf8 kernel locals math
math.combinatorics math.parser prettyprint sequences ;
IN: solution

: read-input ( path -- seq )
    utf8 file-lines
    [ string>number ] map
;

! Return the product of n members that match a given sum k.
:: solve ( seq n k -- prod )
    seq n all-combinations
    [ sum k = ] find product nip
;

"./input" read-input
[ 2 2020 solve . ]
[ 3 2020 solve . ] bi

