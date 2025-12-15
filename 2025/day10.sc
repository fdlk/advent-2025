import common.{Grid, aStarSearch}

import scala.annotation.tailrec
import scala.io.Source
import scala.collection.parallel.CollectionConverters.*
import breeze.linalg.{DenseMatrix, DenseVector, inv}

import scala.util.Try

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

def minPresses(machine: (Set[Int], List[Set[Int]], Array[Int])) = {
  val (_, wirings, joltage) = machine
  val mat = joltage.indices.map(light => wirings.map(wiring => if wiring(light) then 1.0 else 0.0))
  val indices = mat.toSet.map(mat.indexOf(_)).toList
  val A = DenseMatrix(indices.map(mat): _*)
  val b = DenseVector(indices.map(joltage).map(_.toDouble): _*)
  solveMachineAllColumnCombos(A,b)
}

machines.zipWithIndex.filter(m => minPresses(m._1).isEmpty).map(_._2)

//val presses = machines.flatMap(minPresses).sum
// 17769 is too low
