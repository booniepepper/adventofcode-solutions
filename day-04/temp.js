fs = require('fs')
file = fs.readFileSync("./input")
file = fs.readFileSync("./input", "utf8")
file = fs.readFileSync("./input", "utf8")
file
file.replace(/\n\n/\n\n\n/g)
file.replace(/\n\n/g, "\n\n\n")
file.replace(/\n\n/g, "\n\n\n").replace(/\n\n/g, "")
passports = file.replace(/\n\n/g, "\n\n\n").replace(/\n\n/g, "").split("\n")
passports.map(p => p.split(' '))
passports.map(p => p.split(' ').map(e => e.split(':')[0])
(
passports.map(p => p.split(' ').map(e => e.split(':')[0]))
passports.map(p => p.split(' ').map(e => e.split(':')[0])).map(e => new Set(e))
passports.map(p => p.split(' ').map(e => e.split(':')[0])).map(e => new Set(e)).filter(e => ['byr','iyr','eyr','hgt','hcl','ecl','pid'].every(f => e.has(f)))
passports.map(p => p.split(' ').map(e => e.split(':')[0])).map(e => new Set(e)).filter(e => ['byr','iyr','eyr','hgt','hcl','ecl','pid'].every(f => e.has(f))).length
passports = file.replace(/\n\n/g, "\n\n\n").replace(/\n\n/g, " ").split("\n")
passports.map(p => p.split(' ').map(e => e.split(':')[0])).map(e => new Set(e)).filter(e => ['byr','iyr','eyr','hgt','hcl','ecl','pid'].every(f => e.has(f))).length
file
passports = file.replace(/\n/g, " ").replace(/  /g, "\n").split("\n")
passports.map(p => p.split(' ').map(e => e.split(':')[0])).map(e => new Set(e)).filter(e => ['byr','iyr','eyr','hgt','hcl','ecl','pid'].every(f => e.has(f))).length