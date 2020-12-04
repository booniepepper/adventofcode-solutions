#!/usr/bin/env node
const {readFileSync} = require('fs');

const keys = ['byr','iyr','eyr','hgt','hcl','ecl','pid'];
const n = readFileSync("./input", "utf8")
    .replace(/\n/g, " ")
    .replace(/  /g, "\n")
    .split("\n")
    .map(p => p.split(' ')
    .map(e => e.split(':')[0]))
    .map(e => new Set(e))
    .filter(e => keys.every(f => e.has(f)))
    .length;

console.log(`Number of valid passports: ${n}`);

