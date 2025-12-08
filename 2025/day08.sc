import scala.io.Source

val input = Source.fromResource("day08.txt").getLines().toList

case class Point(x: Int, y: Int, z: Int) {
  def distanceSquaredTo(other: Point): Long = {
    val dx = (x - other.x).toLong
    val dy = (y - other.y).toLong
    val dz = (z - other.z).toLong
    dx * dx + dy * dy + dz * dz
  }
}

val boxes = input.map {
  case s"$x,$y,$z" => Point(x.toInt, y.toInt, z.toInt)
}

val connections = boxes.combinations(2).toList.sortBy {
  case List(a, b) => a.distanceSquaredTo(b)
}

def connect(circuits: List[Set[Point]], connection: List[Point]): List[Set[Point]] = {
  circuits.partition(circuit => connection.exists(circuit.contains)) match {
    case (List(a, b), unaffected) => a.union(b) :: unaffected
    case _ => circuits
  }
}

val part1 = connections.take(1000)
  .foldLeft(boxes.map(Set(_)))(connect)
  .map(_.size)
  .sorted
  .takeRight(3)
  .product

val part2Index = connections.iterator
  .scanLeft(boxes.map(Set(_)))(connect)
  .indexWhere(_.size == 1) - 1

val part2 = connections(part2Index).map(_.x.toLong).product