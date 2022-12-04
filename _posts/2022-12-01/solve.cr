lines = File.read "input"

elves = lines.split(/\n\n/).map {|group| group.lines.map(&.to_i).sum}

solution_1 = elves.max

puts solution_1

solution_2 = elves.sort.last(3).sum

puts solution_2

