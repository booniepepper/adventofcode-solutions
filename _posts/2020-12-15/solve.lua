#!/usr/bin/env lua

io.input(io.open("./input", "r"))
ns = {}
lookup = {}
for n in string.gmatch(io.read(), "%d+") do
    n = tonumber(n)
    ns[#ns+1] = n
    lookup[n] = #ns
end

ns[#ns+1] = 0

for i = #ns, 30000000 do
    prev = ns[i]
    curr = lookup[prev]
    if curr == nil then
        curr = 0
    else
        curr = i - curr
    end
    lookup[prev] = i
    ns[i+1] = curr
end

print("2020th: " .. ns[2020])
print("30000000th: " .. ns[30000000])

