import common.{Regex, loadPackets}

import scala.annotation.tailrec
import scala.io.Source

val input = loadPackets(List("day06.txt")).map(_.split("""\s+""").filter(_.nonEmpty).toList)

@tailrec
def mathsPart1 (operations: List[String], operands: List[List[Long]], soFar: Long = 0): Long = operations match {
  case Nil => soFar
  case "+" :: rest => mathsPart1(rest, operands.map(_.tail), soFar + operands.map(_.head).sum)
  case "*" :: rest => mathsPart1(rest, operands.map(_.tail), soFar + operands.map(_.head).product)
}

val part1 = mathsPart1(input.last, input.dropRight(1).map(_.map(_.toLong)))
