import scala.annotation.tailrec
import scala.io.Source
import scala.collection.parallel.CollectionConverters._

val input = Source.fromResource("day10.txt").getLines().toList

val machines = input.map {
  case s"[$light] $wiring {${joltage}}" =>
    (light.zipWithIndex.filter(_._1 == '#').map(_._2).toSet,
      wiring.split(" ").map {case s"($indices)" => indices.split(",").map(_.toInt).toSet}.toList,
      joltage.split(",").map(_.toInt))
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

def press(current: Map[Int, Int], wiring: Set[Int]): Map[Int, Int] =
  wiring.foldLeft(current)
    ((jolts, button) => jolts.updatedWith(button)(_.map(_ + 1).orElse(Some(1))))

def overCharged(current: Map[Int, Int], desired: Map[Int, Int]): Boolean =
  current.exists((light, currentJoltage) => desired.getOrElse(light, 0) < currentJoltage)

@tailrec
def fewestPresses(current: Set[Map[Int, Int]], desired: Map[Int, Int], wirings: List[Set[Int]], n: Int = 0): Int = {
  if (current.contains(desired)) n
  else fewestPresses(
    current.flatMap(c => wirings.map(w => press(c, w)).toSet).filterNot(c => overCharged(c, desired)),
    desired,
    wirings,
    n + 1)
}

val part2 = machines.par.map {
  case (_, wirings, joltage) => fewestPresses(Set(Map()),
    joltage.zipWithIndex.map(_.swap).filter((_, joltage) => joltage > 0).toMap,
    wirings)
}.sum