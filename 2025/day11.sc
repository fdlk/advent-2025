import scala.annotation.tailrec
import scala.io.Source

val input = Source.fromResource("day11.txt").getLines().toList

case class Device(id: String, outputs: List[String]) {}

val devices = input.map {
  case s"$id: $outputs" => id -> Device(id, outputs.split(" ").toList)
}.toMap

type State = Map[String, Int]

@tailrec
def numPaths(state: State = Map("you" -> 1), out: Int = 0): Int = {
  if state.isEmpty
  then out
  else
    val next = state.toList.flatMap {
      case (id, numPaths) => devices(id).outputs.map(output => (output, numPaths))
    }.groupMapReduce(_._1)(_._2)(_+_)
    numPaths(next.filter(_._1 != "out"), next.getOrElse("out", 0) + out)
}

val part1 = numPaths()