USING: io.files io.encodings.utf8 locals kernel math
math.parser prettyprint sequences ;
IN: solution

: read-input ( path -- seq )
    utf8 file-lines
    [ string>number ] map
;

! Return the product of two members that match a given sum.
:: solve2 ( seq sum -- prod? )
    seq length 1 > [
        seq first :> x
        sum x - :> y
        seq rest :> zs

        y zs member? [
            x y *
        ] [
            zs sum solve2
        ] if
    ] [
        f
    ] if
;

! Return the product of three members that match a given sum.
! Will blow up if no such triple exists.
:: solve3 ( seq sum -- prod )
    seq first :> x
    sum x - :> y
    seq rest :> zs
    zs y solve2 :> prod

    prod [
        x prod *
    ] [
        zs sum solve3
    ] if
;


"./input" read-input
[ 2020 solve2 . ]
[ 2020 solve3 . ] bi

