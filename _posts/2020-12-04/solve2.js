#!/usr/bin/env node
const {readFileSync} = require('fs');

// Splitting key from value and parsing ints would
// be more readable (and performant?) for many of these.
const patterns = [
    /\bbyr:(19[2-9]\d|200[012])\b/,
    /\biyr:20(1\d|20)\b/,
    /\beyr:20(2\d|30)\b/,
    /\bhgt:(1([5-8]\d|9[0123])cm|(59|6\d|7[0-6])in)\b/,
    /\bhcl:#[0-9a-f]{6}\b/,
    /\becl:(amb|blu|brn|gry|grn|hzl|oth)\b/,
    /\bpid:\d{9}\b/
];

const validate = pass => patterns.every(patt => pass.match(patt));

const n = readFileSync("./input", "utf8")
    .replace(/\n/g, " ")
    .replace(/  /g, "\n")
    .split("\n")
    .filter(validate)
    .length;

console.log(`Valid passports: ${n}`);

