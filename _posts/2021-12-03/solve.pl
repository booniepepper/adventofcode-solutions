#!/bin/swipl -fq

:- use_module(library(readutil)).

:- initialization main.

main :-
  open('input', read, Fd, []),
  read_string(Fd, _, Str),
  split_string(Str, "\n", "", LinesRaw),
  exclude(is_empty, LinesRaw, Lines),
  length(Lines, Count),
  maplist(string_chars, Lines, Chars),
  maplist(maplist(char_to_digit), Chars, Digits),
  foldl(maplist(plus), Digits, [0,0,0,0,0,0,0,0,0,0,0,0], Sums),
  Threshold is (Count / 2),
  maplist(gamma(Threshold), Sums, GammaBin),
  bin_to_dec(GammaBin, Gamma),
  maplist(epsilon(Threshold), Sums, EpsilonBin),
  bin_to_dec(EpsilonBin, Epsilon),
  Solution1 is Gamma * Epsilon,
  write(Solution1),
  nl,
  halt(0).

gamma(Threshold, Value, Res) :-
  Threshold < Value,
  Res is 1,
  !.
gamma(_, _, 0).

epsilon(Threshold, Value, Res) :-
  Threshold > Value,
  Res is 1,
  !.
epsilon(_, _, 0).

bin_to_dec(Bin, Dec) :-
  bin_to_dec(Bin, 0, Dec).
bin_to_dec([], Dec, Dec) :- !.
bin_to_dec([B|Bin], Prev, Dec) :-
  Next is (Prev * 2 + B),
  bin_to_dec(Bin, Next, Dec).

char_to_digit(Char, Digit) :-
  char_type(Char, digit(Digit)).

is_empty("").
