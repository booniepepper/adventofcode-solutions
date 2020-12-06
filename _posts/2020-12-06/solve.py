#!/usr/bin/env python3
from functools import reduce

groups = open('./input').read().split('\n\n');


def uniq_chars(s):
    char_sets = map(set, s.split('\n'))
    chars = reduce(lambda a, b: a.union(b), char_sets)
    return len(chars)


def common_chars(s):
    char_sets = map(set, s.split('\n'))
    chars = reduce(lambda a, b: a.intersection(b), char_sets)
    return len(chars)


def main():
    print('Total uniq:  ', sum(map(uniq_chars, groups)))
    print('Total common:', sum(map(common_chars, groups)))

main()

