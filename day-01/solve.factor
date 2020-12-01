USING: io.files io.encodings.utf8 locals kernel math
math.parser prettyprint sequences ;
IN: solution

: read-input ( path -- seq )
    utf8 file-lines
    [ string>number ] map
;

! Return the product of two members that match a given sum.
! Will blow up if no such pair exists.
:: solve ( seq sum -- prod )
    seq first :> x
    sum x - :> y
    seq rest :> zs

    y zs member? [
        x y *
    ] [
        zs sum solve
    ] if
;

"./input" read-input 2020 solve pprint

