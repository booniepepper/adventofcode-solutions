#!/usr/bin/env yabasic

sub rotate$(dir$, degrees)
    times = degrees / 90
    while (times < 1)
        times = times + 4
    end while

    while (times > 0)
        switch dir$
            case "N": dir$ = "W" : break
            case "W": dir$ = "S" : break
            case "S": dir$ = "E" : break
            case "E": dir$ = "N" : break
        end switch
        times = times - 1
    end while

    return dir$
end sub

sub solve()
    open #8, "input", "r"

    facing$ = "E"
    dim pos(2)
    pos(1) = 0
    pos(2) = 0

    while !eof(8)
        line input #8 command$

        op$ = left$(command$,1)
        amount = val(mid$(command$,2))

        switch op$
            case "R": facing$ = rotate$(facing$, -amount) : continue
            case "L": facing$ = rotate$(facing$, amount) : continue
        end switch

        move_dir$=facing$
        if (!op$ = "F") move_dir$ = op$

        switch move_dir$
            case "N": pos(1) = pos(1) - amount : break
            case "E": pos(2) = pos(2) + amount : break
            case "S": pos(1) = pos(1) + amount : break
            case "W": pos(2) = pos(2) - amount : break
        end switch
    end while

    print "Ended at (", pos(1), ", ", pos(2), ")"
    print "The Manhattan distance from the start is ", pos(1) + pos(2)
end sub

solve()

