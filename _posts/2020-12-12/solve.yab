#!/usr/bin/env yabasic

sub solution_1()
    facing$ = "E"
    dim pos(2)
    pos(1) = 0
    pos(2) = 0

    open #8, "input", "r"
    while !eof(8)
        line input #8 command$

        op$ = left$(command$,1)
        amount = val(mid$(command$,2))

        switch op$
            case "R": facing$ = cardinal_rotate$(facing$, -amount) : continue
            case "L": facing$ = cardinal_rotate$(facing$, amount) : continue
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
    close #8

    print "Ended at (", pos(1), ", ", pos(2), ")"
    print "The Manhattan distance from the start is ", abs(pos(1)) + abs(pos(2))
end sub

sub cardinal_rotate$(dir$, degrees)
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

sub solution_2()
    dim pos(2)
    pos(1) = 0
    pos(2) = 0
    dim waypoint(2)
    waypoint(1) = -1
    waypoint(2) = 10

    open #8, "input", "r"
    while !eof(8)
        line input #8 command$

        op$ = left$(command$,1)
        amount = val(mid$(command$,2))

        switch op$
            case "R": rotate(waypoint(), -amount) : break
            case "L": rotate(waypoint(), amount) : break
            case "F":
                pos(1) = pos(1) + waypoint(1) * amount
                pos(2) = pos(2) + waypoint(2) * amount
                break
            case "N": waypoint(1) = waypoint(1) - amount : break
            case "E": waypoint(2) = waypoint(2) + amount : break
            case "S": waypoint(1) = waypoint(1) + amount : break
            case "W": waypoint(2) = waypoint(2) - amount : break
        end switch
    end while
    close #8

    print "Ended at (", pos(1), ", ", pos(2), ")"
    print "The Manhattan distance from the start is ", abs(pos(1)) + abs(pos(2))
end sub

sub rotate(xy(), degrees)
    times = degrees / 90
    while (times < 1)
        times = times + 4
    end while

    while (times > 0)
        _temp = xy(1)
        xy(1) = -xy(2)
        xy(2) = _temp

        times = times - 1
    end while
end sub


solution_1()

solution_2()

