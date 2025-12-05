import common.loadPackets

case class IngredientRange(start: Long, end: Long) {
  val size = end - start + 1
  def contains(ingredient: Long): Boolean =
    start <= ingredient && end >= ingredient

  def overlap(other: IngredientRange): Boolean =
    other.contains(start) || other.contains(end) || contains(other.start) || contains(other.end)
}

val input = loadPackets(List("day05.txt")).flatMap({
  case s"${start}-${end}" => Some(IngredientRange(start.toLong, end.toLong))
  case "" => None
  case id => Some(id.toLong)
})

val ranges = input.collect({case range: IngredientRange => range})
val ingredients = input.collect({case ingredient: Long => ingredient})

val part1 = ingredients.count(ingredient => ranges.exists(_.contains(ingredient)))

def combine(ranges: List[IngredientRange]): IngredientRange =
  IngredientRange(ranges.map(_.start).min, ranges.map(_.end).max)

val combinedRanges = ranges.foldLeft(List[IngredientRange]())
  ((soFar, range) => soFar.partition(_.overlap(range)) match {
    case (overlapping, separate) => combine(range :: overlapping) :: separate
  })

val part2 = combinedRanges.map(_.size).sum