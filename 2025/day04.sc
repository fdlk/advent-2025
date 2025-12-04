import common.loadPackets

import scala.annotation.tailrec

val input = loadPackets(List("day04.txt")).toArray

case class Point(x: Int, y: Int) {
    def neighbors = for {
        dx <- -1 to 1
        dy <- -1 to 1
        result = Point(x + dx, y + dy)
        if result != this
    }  yield result
}

val rolls: Set[Point] = (for {
    y <- input.indices
    x <- input.head.indices
    if input(x)(y) == '@'
}
yield Point(x, y)).toSet

val part1 = rolls.count(_.neighbors.count(rolls) < 4)

@tailrec
def clearRolls(rolls: Set[Point]): Set[Point] =
    val clearable = rolls.filter(_.neighbors.count(rolls) < 4)
    if (clearable.isEmpty)
        rolls
    else
        clearRolls(rolls -- clearable)
        
val  part2 = rolls.size - clearRolls(rolls).size