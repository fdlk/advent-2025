import scala.annotation.tailrec
import scala.io.Source
import scala.collection.parallel.CollectionConverters._

val input = Source.fromResource("day10.txt").getLines().toList

val machines = input.map {
  case s"[$light] $wiring {${joltage}}" =>
    (light.zipWithIndex.filter(_._1 == '#').map(_._2).toSet,
      wiring.split(" ").map {case s"($indices)" => indices.split(",").map(_.toInt).toSet}.toList,
      joltage.split(",").map(_.toInt).toSet)
}

def toggle(current: Set[Int], wiring: Set[Int]): Set[Int] =
  (current -- wiring) ++ (wiring -- current)

@tailrec
def fewestToggles(current: Set[Set[Int]], desired: Set[Int], wirings: List[Set[Int]], n: Int = 0): Int = {
  if (current.contains(desired)) n
  else fewestToggles(current.flatMap(c => wirings.map(w => toggle(c, w)).toSet), desired, wirings, n + 1)
}

val part1 = machines.map {
  case (lights, wirings, _) => fewestToggles(Set(Set()), lights, wirings)
}.sum

