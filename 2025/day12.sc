import scala.annotation.tailrec
import scala.io.Source

val input = Source.fromResource("day12.txt").getLines().toList

case class Point(x: Int, y: Int) {
  def rotate: Point = Point(2 - y, x)

  def flip: Point = Point(y, x)

  def plus(other: Point, width: Int, height: Int) = Some(Point(x + other.x, y + other.y))
    .filter(p => p.x < width && p.y < height)

  def next(width: Int, height: Int): Option[Point] =
    if x < width - 1
    then Some(copy(x = x + 1))
    else if y < height - 1
    then Some(copy(x = 0, y = y + 1))
    else None
}

case class Present(index: Int, points: Set[Point]) {
  def rotate: Present = Present(index, points.map(_.rotate))

  def flip: Present = Present(index, points.map(_.flip))

  lazy val forms = Set(this, this.rotate, this.rotate.rotate, this.rotate.rotate.rotate)
    .flatMap(p => Set(p, p.flip))

  override def toString: String =
    s"$index:\n" + (for y <- 0 to 2
      yield (for x <- 0 to 2 yield if points(Point(x, y)) then '#' else '.').mkString)
      .mkString("\n")
}

case object Presents {
  def parse(id: Int, representation: List[String]): Present =
    Present(id,
      (for x <- 0 to 2
           y <- 0 to 2
           if representation(y).charAt(x) == '#'
      yield Point(x, y)).toSet)
}

def parsePresentShapes(input: List[String]): List[Present] = input.span(_.nonEmpty) match {
  case (s"$index:" :: present, rest) => Presents.parse(index.toInt, present) :: parsePresentShapes(rest.tail)
  case _ => Nil
}

val shapes = parsePresentShapes(input)

case class State(width: Int,
                 height: Int,
                 presents: Map[Int, Int],
                 covered: Set[Point] = Set(),
                 cursor: Point = Point(0, 0)) {
  def placePresent: Iterable[State] = presents.flatMap { case (id, count) =>
    val updatedCount = presents.updatedWith(id)(v => v.map(c => c - 1).filter(_ > 0))
    shapes(id)
      .forms
      .flatMap(present => {
        val presentPoints = present.points.map(_.plus(cursor, width, height))
        if (presentPoints.forall(p => p.nonEmpty && !covered.contains(p.get))) {
          Some(copy(covered = covered ++ presentPoints.flatten, presents = updatedCount))
        } else None
      })
  }

  def moveCursor: Option[State] = cursor.next(width, height).map(c => copy(cursor = c))

  val solved: Boolean = presents.isEmpty

  def slack: Int = {
    val tilesLeft: Int = (height - cursor.y) * width + width - cursor.x
    val tilesNeeded: Int = presents.map((id, count) => shapes(id).points.size * count).sum
    tilesLeft - tilesNeeded
  }

  def trickiness: Float = slack.toFloat / (width * height)

  def solvable: Boolean = slack >= 0
}

def getInitialState(line: String): State = line match {
  case s"${width}x${height}: ${counts}" => State(
    width.toInt,
    height.toInt,
    counts.split(" ").toList.zipWithIndex.filter(_._1 != "0").map {
      case (countString, id) => id -> countString.toInt
    }.toMap)
}

def canPlacePresents(state: State, until: Long): Boolean = {
  if(System.currentTimeMillis() > until)
    false
  else state.solved ||
  state.solvable && (state.placePresent.exists(canPlacePresents(_, until)) || state.moveCursor.exists(canPlacePresents(_, until)))
}

val lines = input.drop(shapes.size * 5)
val until = System.currentTimeMillis() + 10000
val part1 = lines.map(getInitialState).sortBy(-_.trickiness).count(canPlacePresents(_, until))
