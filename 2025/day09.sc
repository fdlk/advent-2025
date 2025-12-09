import scala.io.Source

val input = Source.fromResource("day09.txt").getLines().toList

case class Tile(x: Int, y: Int) {
  def areaSpanned(other: Tile): Long = {
    val dx = (x - other.x + 1).abs
    val dy = (y - other.y + 1).abs
    dx.toLong * dy
  }
}

val red = input.map {
  case s"$x,$y" => Tile(x.toInt, y.toInt)
}

val part1 = red.combinations(2).map {
  case List(a, b) => a.areaSpanned(b)
}.max