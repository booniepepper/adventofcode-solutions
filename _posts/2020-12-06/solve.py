#!/usr/bin/env python3
from functools import reduce

groups = open('./input').read().split('\n\n');


def count_using(f, groups):
    def count(s):
        sets = map(set, s.split('\n'))
        elems = reduce(f, sets)
        return len(elems)
    return sum(map(count, groups))


def main():
    union = lambda a,b: a.union(b)
    print('Total (uniq answers):', count_using(union, groups))

    intersection = lambda a,b: a.intersection(b)
    print('Total (same answers):', count_using(intersection, groups))


main()

