#!/usr/bin/env -S swipl -fq
:- use_module(library(clpfd)).
:- initialization main.

main :-
    file_lines("input", Trees),

    solve_1(Trees, Sum),
    writeln(Sum),

    halt.

% --- Misc utility ---

rotate(Goal, As, Bs) :-
    transpose(As, AsRot),
    call(Goal, AsRot, BsRot),
    transpose(BsRot, Bs).

mirror(Goal, As, Bs) :-
    maplist(reverse, As, AsRev),
    call(Goal, AsRev, BsRev),
    maplist(reverse, BsRev, Bs).

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
    maplist(visible_from_w(-1), Trees, Visibles).

visible_from_w(Tallest, [Tree|Trees], Visibles) :-
    Tree > Tallest,
    visible_from_w(Tree, Trees, RestVisibles),
    Visibles = [1 | RestVisibles];
    visible_from_w(Tallest, Trees, RestVisibles),
    Visibles = [0 | RestVisibles].
visible_from_w(_, _, []).

% --- Reading the file into a number matrix ---

file_lines(File, Matrix) :-
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

