#!/usr/bin/env kscript

import java.io.File

val lines = File("input").readLines()

val crateLines = lines.takeWhile { it != "" }
val crates = (1..34 step 4).map { n ->
        crateLines
            .map { it[n] }
            .filter { it != ' ' }
    }

data class Command(val n: Int, val from: Int, val to: Int)

val commands = lines.dropWhile { it != "" }
    .filter { it != "" }
    .map { it.split(' ') }
    .map { Command(it[1].toInt(), it[3].toInt(), it[5].toInt()) }

val crates1 = crates.map { ArrayDeque(it) }.toMutableList()

for (command in commands) {
    val (n, from, to) = command
    for (_i in 1..n) {
        crates1[to-1].addFirst(crates1[from-1].removeFirst())
    }
}

println(crates1.map { it.first() }.joinToString(""))

val crates2 = crates.map { it.toMutableList() }.toMutableList()

for (command in commands) {
    val (n, from, to) = command

    var head = crates2[from-1].take(n).toMutableList()
    head.addAll(crates2[to-1])
    crates2[to-1] = head

    var tail = crates2[from-1].drop(n).toMutableList()
    crates2[from-1] = tail
}

println(crates2.map { it.first() }.joinToString(""))

