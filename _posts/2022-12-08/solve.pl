#!/usr/bin/env -S swipl -fq
:- use_module(library(clpfd)).
:- initialization main.

main :-
    file_matrix("input", Trees),

    solve_1(Trees, Sum),
    writeln(Sum),

    solve_2(Trees, BestScore),
    writeln(BestScore),

    halt.

% --- Utility ---

rotate(Goal, As, Bs) :-
    transpose(As, AsRot), 
    call(Goal, AsRot, BsRot),
    transpose(BsRot, Bs).

mirror(Goal, As, Bs) :-
    maplist(reverse, As, AsRev),
    call(Goal, AsRev, BsRev),
    maplist(reverse, BsRev, Bs).

split(0, [It|After], [], It, After).
split(N, [Item|List], Before, It, After) :-
    NextN is N - 1,
    split(NextN, List, RestBefore, It, After),
    Before = [Item|RestBefore].

visible(Tallest, [Tree|Trees], Visibles) :-
    (   Tree > Tallest
    ->  visible(Tree, Trees, RestVisibles),
        Visibles = [1 | RestVisibles]
    ;   visible(Tallest, Trees, RestVisibles),
        Visibles = [0 | RestVisibles]
    ).
visible(_, _, []).

% --- Stuff for part 1 ---

solve_1(Trees, Sum) :-
    visible_from_w(Trees, W),
    visible_from_e(Trees, E),
    rotate(visible_from_w, Trees, N),
    rotate(visible_from_e, Trees, S),
    count_visibles(N, E, S, W, Sum).

count_visibles(A, B, C, D, Sum) :-
    maplist(merge_visibles, A, B, C, D, Merged),
    maplist(sum_list, Merged, RowSums),
    sum_list(RowSums, Sum).

merge_visibles([A|As], [B|Bs], [C|Cs], [D|Ds], [N|Ns]) :-
        merge_visibles(As, Bs, Cs, Ds, Ns),
        (member(1, [A,B,C,D]) -> N is 1 ; N is 0 ).
merge_visibles([], [], [], [], []).

visible_from_e(Trees, Visibles) :-
    mirror(visible_from_w, Trees, Visibles).

visible_from_w(Trees, Visibles) :-
    maplist(visible(-1), Trees, Visibles).

% --- Stuff for Part 2 ---

take_until(_, [], []).
take_until(Goal, [A|As], Bs) :-
    (   call(Goal, A)
    ->  Bs = [A]
    ;   take_until(Goal, As, RestBs),
        Bs = [A | RestBs]
    ).

solve_2(Trees, Score) :-
    nth0(0, Trees, Row1),
    length(Row1, NCol),
    length(Trees, NRow),
    LastCol is NCol - 2,
    LastRow is NRow - 2,
    scenic_score([1, 1], [LastRow, LastCol], 0, Trees, Score).

scenic_score([Row, Col], [LastRow, LastCol], PrevScore, Trees, BestScore) :-
    split(Row, Trees, AboveRows, TreeRow, BelowRows),
    maplist(nth0(Col), AboveRows, AboveRev),
    reverse(AboveRev, Above),
    maplist(nth0(Col), BelowRows, Below),
    split(Col, TreeRow, BeforeRev, It, After),
    reverse(BeforeRev, Before),
    take_until(=<(It), Above, Ns),
    take_until(=<(It), Below, Ss),
    take_until(=<(It), Before, Es),
    take_until(=<(It), After, Ws),
    length(Ns, N),
    length(Ss, S),
    length(Es, E),
    length(Ws, W),

    Score is N * S * E * W,

    (   Score > PrevScore
    ->  NextScore = Score
    ;   NextScore = PrevScore
    ),

    (   Col < LastCol
    ->  NextCol is Col + 1,
        scenic_score([Row, NextCol], [LastRow, LastCol], NextScore, Trees, BestScore)
    ;   (   Row < LastRow
        ->  NextRow is Row + 1,
            scenic_score([NextRow, 0], [LastRow, LastCol], NextScore, Trees, BestScore)
        ;   BestScore = NextScore
        )
    ).

% --- Reading the file into a number matrix ---

file_matrix(File, Matrix) :-
    setup_call_cleanup(open(File, read, In),
       stream_matrix(In, Matrix),
       close(In)).

stream_matrix(In, Matrix) :-
    read_string(In, _, Str),
    split_string(Str, "\n", "", Lines),
    exclude(=(""), Lines, Lines2),
    maplist(digits_numbers, Lines2, Matrix).

digits_numbers(String, Numbers) :-
    string_codes(String, Codes),
    maplist(number_code, Numbers, Codes).

number_code(Number, Code) :-
    number_codes(Number, [Code]).

