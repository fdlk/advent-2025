import common.loadPackets

import scala.annotation.tailrec

val input = loadPackets(List("day03.txt"))
  .map(_.toList.map(_ - '0').map(_.toLong))

def joltage(bank: List[Long], digitsLeft: Int): Long =
  if (digitsLeft == 1)
    bank.max
  else
    val max = bank.dropRight(digitsLeft - 1).max
    Math.pow(10, digitsLeft-1).toLong * max + joltage(bank.drop(bank.indexOf(max) + 1), digitsLeft - 1)

val part1 = input.map(joltage(_, 2)).sum
val part2 = input.map(joltage(_, 12)).sum
