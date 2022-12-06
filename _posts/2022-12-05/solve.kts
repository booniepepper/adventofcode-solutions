#!/usr/bin/env kscript

import java.io.File

val lines = File("input").readLines()

val crateLines = lines.takeWhile { it != "" }
val crates = (1..34 step 4).map { n ->
        crateLines
            .map { it[n] }
            .filter { it != ' ' }
    }
    .map { ArrayDeque(it) }

data class Command(val n: Int, val from: Int, val to: Int)

val commands = lines.dropWhile { it != "" }
    .filter { it != "" }
    .map { it.split(' ') }
    .map { Command(it[1].toInt(), it[3].toInt(), it[5].toInt()) }

for (command in commands) {
    val (n, from, to) = command
    for (_i in 1..n) {
        crates[to-1].addFirst(crates[from-1].removeFirst())
    }
}

println(crates.map { it.first() }.joinToString(""))

