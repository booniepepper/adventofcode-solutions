validate = f => [/byr:(19[2-9]\d|200[012])/, /iyr:20(1\d|20)/, /eyr:20(2\d|30)/, /hgt:(1([5-8]\d|9[0123])cm|(59|6\d|7[0-6])in)/, /hcl:#[0-9a-f]{6}/, /ecl:(amb|blu|brn|gry|grn|hzl|oth)/, /pid:\d{9}/].every(f => e.match(f))
validate("eyr:1972 cid:100
hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926
validate("eyr:1972 cid:100 
validate("eyr:1972 cid:100 hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926
validate = e => [/byr:(19[2-9]\d|200[012])/, /iyr:20(1\d|20)/, /eyr:20(2\d|30)/, /hgt:(1([5-8]\d|9[0123])cm|(59|6\d|7[0-6])in)/, /hcl:#[0-9a-f]{6}/, /ecl:(amb|blu|brn|gry|grn|hzl|oth)/, /pid:\d{9}/].every(f => e.match(f))
validate("eyr:1972 cid:100 hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926")
validate = e => [/\bbyr:(19[2-9]\d|200[012])\b/, /\biyr:20(1\d|20)\b/, /\beyr:20(2\d|30)\b/, /\bhgt:(1([5-8]\d|9[0123])cm|(59|6\d|7[0-6])in)\b/, /\bhcl:#[0-9a-f]{6}\b/, /\becl:(amb|blu|brn|gry|grn|hzl|oth)\b/, /\bpid:\d{9}\b/].every(f => e.match(f))
validate("eyr:1972 cid:100 hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926")
file = fs.readFileSync("./input", "utf8")
passports = file.replace(/\n/g, " ").replace(/  /g, "\n").split("\n")
passports.filter(validate).length