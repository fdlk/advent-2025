import scala.annotation.tailrec
import scala.io.Source

val input = Source.fromResource("day09.txt").getLines().toList

case class Tile(x: Int, y: Int) {
  def areaSpanned(other: Tile): Long = {
    val dx = (x - other.x).abs + 1
    val dy = (y - other.y).abs + 1
    dx.toLong * dy
  }

  def areaTiles(other: Tile): Seq[Tile] = for {
    xx <- other.x.min(x) to other.x.max(x)
    yy <- other.y.min(y) to other.y.max(y)
  }
  yield Tile(xx, yy)

  def neighbors = for {
    dx <- -1 to 1
    dy <- -1 to 1
    result = Tile(x + dx, y + dy)
    if result != this
  } yield result
}

val red = input.map {
  case s"$x,$y" => Tile(x.toInt, y.toInt)
}

val part1 = red.combinations(2).map {
  case List(a, b) => a.areaSpanned(b)
}.max

def getRanges(items: List[Int]): List[Range] = {
  val values = items.distinct.sorted
  values.map(x => x to x).concat(
      values.sliding(2).map {
        case List(a, b) => (a + 1) until b
      }.filter(_.nonEmpty))
  .sortBy(_.head)
}

val xRanges = getRanges(red.map(_.x))
val yRanges = getRanges(red.map(_.y))

def zoomOut(p: Tile) =
  Tile(xRanges.indexWhere(_.contains(p.x)), yRanges.indexWhere(_.contains(p.y)))

val zoomedOutTiles = red.map(zoomOut)

val edge = (zoomedOutTiles.last :: zoomedOutTiles).sliding(2).flatMap {
  case List(from, to) => from.areaTiles(to)
}.toSet

@tailrec
def floodFill(open: Set[Tile], closed: Set[Tile] = edge): Set[Tile] = {
  if open.isEmpty
  then closed
  else
    val combined = open ++ closed
    floodFill(open.flatMap(_.neighbors).diff(combined), combined)
}

val startTile = zoomOut(red.head)
val redAndGreen = floodFill(Set(Tile(477,246)))

val part2 = red.zip(zoomedOutTiles).combinations(2).filter {
  case List((_, zoomedA), (_, zoomedB)) => zoomedA.areaTiles(zoomedB).forall(redAndGreen)
}.map {
  case List((a,_), (b, _)) => a.areaSpanned(b)
}.max