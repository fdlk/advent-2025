import scala.annotation.tailrec
import scala.io.Source

val input = Source.fromResource("day11.txt").getLines().toList

val devices = input.map {
  case s"$id: $outputs" => id -> outputs.split(" ").toList
}.toMap

type State = Map[String, Int]

@tailrec
def numPaths(state: State, target: String, found: Int = 0): Int = {
  if state.isEmpty
  then found
  else
    val next = state.toList.flatMap {
      case (id, numPaths) => devices(id).map(output => (output, numPaths))
    }.groupMapReduce(_._1)(_._2)(_ + _)
    numPaths(
      next.filterNot { case (id, _) => Set(target, "out").contains(id) },
      target,
      next.getOrElse(target, 0) + found
    )
}

val part1 = numPaths(Map("you" -> 1), "out")

numPaths(Map("dac" -> 1), "fft") == 0

List(
  numPaths(Map("svr" -> 1), "fft"),
  numPaths(Map("fft" -> 1), "dac"),
  numPaths(Map("dac" -> 1), "out"))
  .map(_.toLong).product