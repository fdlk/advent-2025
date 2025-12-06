import scala.annotation.tailrec
import scala.io.Source

val input = Source.fromResource("day06.txt").getLines().toList

@tailrec
def mathsPart1 (operations: List[String], operands: List[List[Long]], soFar: Long = 0): Long = operations match {
  case Nil => soFar
  case "+" :: rest => mathsPart1(rest, operands.map(_.tail), soFar + operands.map(_.head).sum)
  case "*" :: rest => mathsPart1(rest, operands.map(_.tail), soFar + operands.map(_.head).product)
}

val inputPart1 = input.map(_.split("""\s+""").filter(_.nonEmpty).toList)
val part1 = mathsPart1(inputPart1.last, inputPart1.dropRight(1).map(_.map(_.toLong)))

def solve(problem: List[List[Char]]) =
  val operands = problem.map(_.dropRight(1).mkString.trim.toLong)
  val operator = problem.map(_.last).mkString.trim
  operator match {
    case "*" => operands.product
    case "+" => operands.sum
  }

@tailrec
def mathsPart2(input: List[List[Char]], soFar: Long = 0): Long = input match {
  case Nil => soFar
  case _ =>
    val (problem, rest) = input.span(_.mkString.trim.nonEmpty)
    mathsPart2(rest.dropWhile(_.mkString.trim.isEmpty), solve(problem) + soFar)
}

val part2 = mathsPart2(input.map(_.toList).transpose)