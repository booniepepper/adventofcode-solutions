#!/usr/bin/env ruby

input = File.open("./input")
    .readlines
    .map(&:chomp)
    .reject(&:empty?)

def n_trees input, dx, dy
    input
        .select.with_index{|_, i| i % dy == 0}
        .map.with_index{|line,i| line[i * dx % line.length]}
        .count{|c| c == '#'}
end

coords = [
    {dx: 1, dy: 1},
    {dx: 3, dy: 1},
    {dx: 5, dy: 1},
    {dx: 7, dy: 1},
    {dx: 1, dy: 2}
]

final = coords.map do |slope|
        n = n_trees(input, slope[:dx], slope[:dy])
        puts "Right #{slope[:dx]}, down #{slope[:dy]}: #{n} trees"
        n
    end
    .reduce(:*)

puts "Final product: #{final}"

