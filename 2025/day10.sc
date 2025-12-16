import common.{Grid, Memo, aStarSearch}

import scala.annotation.tailrec
import scala.io.Source
import scala.collection.parallel.CollectionConverters.*
import breeze.linalg.{DenseMatrix, DenseVector, inv}

import scala.collection.MapView
import scala.util.Try

val input = Source.fromResource("day10.txt").getLines().toList

val machines = input.map {
  case s"[$light] $wiring {${joltage}}" =>
    (light.zipWithIndex.filter(_._1 == '#').map(_._2).toSet,
      wiring.split(" ").map { case s"($indices)" => indices.split(",").map(_.toInt).toSet }.toList,
      joltage.split(",").map(_.toInt))
}

def toggle(current: Set[Int], wiring: Set[Int]): Set[Int] =
  (current -- wiring) ++ (wiring -- current)

@tailrec
def fewestToggles(current: Set[Set[Int]], desired: Set[Int], wirings: List[Set[Int]], n: Int = 0): Int = {
  if current.contains(desired) then n
  else fewestToggles(current.flatMap(c => wirings.map(w => toggle(c, w)).toSet), desired, wirings, n + 1)
}

val part1 = machines.map {
  case (lights, wirings, _) => fewestToggles(Set(Set()), lights, wirings)
}.sum

def solveMachineAllColumnCombos(A: DenseMatrix[Double], b: DenseVector[Double]): Option[Int] = {
  val m = A.cols
  val candidates = (A.rows to 0 by -1).flatMap(n => {
    val colCombinations = (0 until m).toList.combinations(n)
    colCombinations.flatMap { cols =>
      val freeCols = (0 until m).filterNot(cols.contains)
      val Ared = A(::, cols).toDenseMatrix
      Try(Ared \ b).toOption match {
        case Some(xRed) =>
          if (xRed.forall(_ >= 0.001)) {
            val xRounded = xRed.map(v => math.round(v).toInt)
            val xFull = DenseVector.zeros[Double](m)
            cols.zipWithIndex.foreach { case (c, idx) => xFull(c) = xRounded(idx) }
            val diff: DenseVector[Double] = (A * xFull) - b
            if diff.toScalaVector.map(_.abs).sum < 0.001 then
              Some(xFull) else None
          } else None
        case None => None
      }
    }
  })
  candidates.toList.map(_.toScalaVector.sum.toInt).sorted.headOption
}

def push(wiring: Map[Int, Int], desired: Map[Int, Int]): Option[Map[Int, Int]] =
    val result = wiring.foldLeft(desired) {
      case (left, (light, amount)) => left.updatedWith(light)(_.map(_ - amount))
    }
    Some(result).filter(_.values.forall(value => value >= 0 && value % 2 == 0))
      .map(_.mapValues(_ / 2).toMap)

case class Machine(wirings: List[Set[Int]]):
  val subsets: List[(Int, Map[Int, Int])] = wirings.toSet.subsets().toList
    .map(subset => (subset.size, subset.toList.flatMap(_.toList).groupMapReduce[Int, Int](a => a)(a => 1)(_ + _)))
  def jolt(desired: Map[Int, Int]): Option[Int] = {
    if desired.forall(_._2 == 0) then Some(0)
    else {
      val foo: List[Int] = subsets
        .flatMap(subset => push(subset._2, desired).map((_, subset._1)))
        .distinct
        .flatMap {
          case (next, cost) => memo.apply(next).map(pushes => 2 * pushes + cost)
        }
      if (foo.isEmpty) None else Some(foo.min)
    }
  }

  def minPresses(joltage: Array[Int]) = {
    val mat = joltage.indices.map(light => wirings.map(wiring => if wiring(light) then 1.0 else 0.0))
    val indices = mat.toSet.map(mat.indexOf(_)).toList
    val A = DenseMatrix(indices.map(mat): _*)
    val b = DenseVector(indices.map(joltage).map(_.toDouble): _*)
    solveMachineAllColumnCombos(A, b)
  }

  val memo = Memo(jolt)

val part2 = machines.zipWithIndex.map {
  case ((_, wirings, desired), id) =>
    val machine = Machine(wirings)
    (id, machine.jolt(desired.zipWithIndex.map(_.swap).toMap),
      machine.minPresses(desired))
}

part2.map(_._2.get).sum
