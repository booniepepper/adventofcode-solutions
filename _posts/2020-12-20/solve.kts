#!/usr/bin/env kotlin

import java.io.File
import java.math.BigInteger

class Tile(raw: String) {
    val n: BigInteger
    val borders: List<String>
    val rborders: List<String>
    val tile: List<String>

    init {
        val lines = raw.split("\n")
        n = lines[0].replace(Regex("\\D"), "").toBigInteger()
        tile = lines.drop(1).take(10)
        borders = listOf(
            tile[0],
            tile.map { it[9] }.joinToString(""),
            tile.drop(9)[0].reversed(),
            tile.map { it[0] }.joinToString("").reversed(),
        )
        rborders = borders.map { it.reversed() }.reversed()
    }
}

fun edgesOf(tiles: List<Tile>): Set<String> =
    tiles.flatMap { it.borders + it.rborders }
        .groupBy { it }
        .filterValues { it.size == 1 }
        .keys

fun cornersOf(tiles: List<Tile>, edges: Set<String> = edgesOf(tiles)): List<Tile> =
    tiles.filter { it.borders.intersect(edges).size == 2 }

val tiles: List<Tile> = File("input").readText()
    .split("\n\n")
    .filter { !it.isEmpty() }
    .map { Tile(it) }
val edges: Set<String> = edgesOf(tiles)
val solutionOne: BigInteger = cornersOf(tiles, edges).map { it.n }
    .fold(BigInteger.ONE, { a, b -> a * b })

// Part 1 Solution
println("Product of corner tile numbers: $solutionOne")

fun seaify(seas: List<Tile>, startingCorner: Tile): List<String> {
    val seaMap: MutableMap<BigInteger, Tile> = seas.associateBy { it.n }.toMutableMap()
    seaMap.remove(startingCorner.n)
    while (seaMap.isNotEmpty()) {
        seas.mapIndexedNotNull { i, it ->
            if ((it.borders + it.rborders).intersect(startingCorner.borders).size == 1 &&
                it.n != startingCorner.n
            ) { Pair(i, it) } else { null }
        }
        break
    }
    return listOf()
}

val seas: List<Tile> = tiles.filter { it.borders.intersect(edges).size == 0 }
val seaCorner: Tile = cornersOf(seas)[0]
println(seas)

println(seaify(seas, seaCorner))
