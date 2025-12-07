import scala.io.Source

val input = Source.fromResource("day07.txt").getLines().toList

val part1 = input.tail.foldLeft[(Set[Int], Int)]((Set(input.head.indexOf('S')), 0))((soFar, line) => soFar match {
  case (beams, splitCount) =>
    val (split, unsplit) = beams.partition(line.charAt(_) == '^')
    (unsplit ++ split.flatMap((i: Int) => Set(i - 1, i + 1)), splitCount + split.size)
})._2