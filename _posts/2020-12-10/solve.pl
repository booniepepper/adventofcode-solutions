#!/usr/bin/env perl
use strict;
use warnings;

open(my $in,  '<',  'input');

my @lines = sort {$a <=> $b} grep {$_ =~ /\d+/} (<$in>);

my @diffs = (0, 0, 0, 1);

my $prev = 0;
for my $line (@lines) {
    my $diff = $line - $prev;
    $diffs[$line - $prev] += 1;
    $prev = $line;
}

# Part 1 solution
print "Differences of ones ($diffs[1]) ";
print "* those of threes ($diffs[3]) ";
print '= ', $diffs[1] * $diffs[3], "\n";

unshift(@lines, 0);
unshift(@lines, -3);
unshift(@lines, -6);
my @routes = (1, 1, 1);

for my $n (3..scalar(@lines-1)) {
    my $routes_here = 0;
    for my $m (1..3) {
        if ($lines[$n] - $lines[$n - $m] <= 3) {
            $routes_here += $routes[$n - $m];
        }
    }
    $routes[$n] = $routes_here;
}

# Part 2 solution
print "Total number of distinct arrangements: ", pop(@routes), "\n";

1;

