#!/usr/bin/env node
const {readFileSync} = require('fs');

// Splitting key from value and parsing ints would
// be more readable (and performant?) for many of these.
const patterns = [
    /byr:(19[2-9]\d|200[012])/,
    /iyr:20(1\d|20)/,
    /eyr:20(2\d|30)/,
    /hgt:(1([5-8]\d|9[0123])cm|(59|6\d|7[0-6])in)/,
    /hcl:#[0-9a-f]{6}/,
    /ecl:(amb|blu|brn|gry|grn|hzl|oth)/,
    /pid:\d{9}/
];

const validate = pass => patterns.every(pass => e.match(p));

const n = readFileSync("./input", "utf8")
    .replace(/\n/g, " ")
    .replace(/  /g, "\n").split("\n")
    .filter(validate)
    .length;

console.log(`Valid passports: ${n}`);

