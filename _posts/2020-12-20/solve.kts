#!/usr/bin/env kotlin

import java.io.File
import java.math.BigInteger

class Tile(raw: String) {
    val n: BigInteger
    val borders: List<String>
    val rborders: List<String>

    init {
        val lines = raw.split("\n")
        n = lines[0].replace(Regex("\\D"), "").toBigInteger()
        borders = listOf(
            lines.drop(1)[0],
            lines.drop(1).take(10).map { it[9] }.joinToString(""),
            lines.drop(10)[0].reversed(),
            lines.drop(1).take(10).map { it[0] }.joinToString("").reversed(),
        )
        rborders = borders.map { it.reversed() }.reversed()
    }
}

val tiles: List<Tile> = File("input").readText()
    .split("\n\n")
    .filter { !it.isEmpty() }
    .map { Tile(it) }
val edges: Set<String> = tiles.flatMap { it.borders + it.rborders }
    .groupBy { it }
    .filterValues { it.size == 1 }
    .keys
val corners: List<Tile> = tiles.filter { it.borders.intersect(edges).size == 2 }

val solutionOne: BigInteger = corners.map { it.n }
    .fold(BigInteger.ONE, { a, b -> a * b })

println("Product of corner tile numbers: $solutionOne")
