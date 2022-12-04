lines = File.read "input"

solution_1 = lines.split(/\n\n/).map {|group| group.lines.map(&.to_i).sum}.max

puts solution_1

