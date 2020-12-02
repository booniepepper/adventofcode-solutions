#!/usr/bin/env escript

-mode(compile).

main(_) ->
    {ok, InputRaw} = file:read_file("./input"),
    Lines = lists:map(fun(Bin) -> parse(binary_to_list(Bin)) end, string:split(InputRaw, "\n", all)),
    io:format("Valid by old method: ~w~n", [solve_old(Lines)]),
    io:format("Valid by new method: ~w~n", [solve_new(Lines)]).

solve_old(Lines) -> solve_old(Lines, 0).

solve_old([], Acc) -> Acc;
solve_old([none|Rest], Acc) -> solve_old(Rest, Acc);
solve_old([{Min, Max, Target, Text} | Rest], Acc) ->
    Count = length(lists:filter(fun(C) -> C =:= Target end, Text)),
    Inc = if (Min =< Count) andalso (Count =< Max) -> 1;
        true -> 0
    end,
    solve_old(Rest, Acc + Inc).

solve_new(Lines) -> solve_new(Lines, 0).

solve_new([], Acc) -> Acc;
solve_new([none|Rest], Acc) -> solve_new(Rest, Acc);
solve_new([{Index1, Index2, Target, Text} | Rest], Acc) ->
    Char1 = lists:nth(Index1, Text),
    Char2 = lists:nth(Index2, Text),
    Inc = if (Char1 =:= Target) xor (Char2 =:= Target) -> 1;
        true -> 0
    end,
    solve_new(Rest, Acc + Inc).

parse("") -> none;
parse(Line) ->
    {A, Line1} = string:to_integer(Line),
    {B, Line2} = string:to_integer(string:prefix(Line1, "-")),
    [Char | Line3] = string:next_grapheme(string:prefix(Line2, " ")),
    Text = string:prefix(Line3, ": "),
    {A, B, Char, Text}.

