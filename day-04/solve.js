#!/usr/bin/env node

const {readFileSync} = require('fs');

n = readFileSync("./input", "utf8")
    .replace(/\n/g, " ")
    .replace(/  /g, "\n")
    .split("\n")
    .map(p => p.split(' ')
    .map(e => e.split(':')[0]))
    .map(e => new Set(e))
    .filter(e => ['byr','iyr','eyr','hgt','hcl','ecl','pid'].every(f => e.has(f)))
    .length

console.log(`Number of valid passports: ${n}`);

