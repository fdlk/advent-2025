import common.{Grid, aStarSearch}

import scala.annotation.tailrec
import scala.io.Source
import scala.collection.parallel.CollectionConverters.*

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

def grid(wirings: List[Set[Int]], desired: Map[Int, Int]) = {
  val stepsize = wirings.map(_.size).max
  new Grid[Map[Int, Int]]:
    override def heuristicDistanceToFinish(from: Map[Int, Int]): Int =
      from.map((light, currentJoltage) => (desired.getOrElse(light, 0) - currentJoltage).abs).sum / stepsize
    override def getNeighbours(state: Map[Int, Int]): Iterable[Map[Int, Int]] =
      wirings.map(w => press(state, w)).filterNot(c => overCharged(c, desired))
    override def moveCost(from: Map[Int, Int], to: Map[Int, Int]): Int = 1
}

val part2 = machines.tail.take(1).map {
  case (_, wirings, joltage) =>
    val desired = joltage.zipWithIndex.map(_.swap).filter((_, joltage) => joltage > 0).toMap
    aStarSearch(Map(), grid(wirings, desired), _ == desired).get._1
}