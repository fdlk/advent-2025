import scala.io.Source

val input = Source.fromResource("day07.txt").getLines().toList

val part1 = input.tail.foldLeft[(Set[Int], Int)]((Set(input.head.indexOf('S')), 0))((soFar, line) =>
  soFar match {
    case (beams, splitCount) =>
      val (split, unsplit) = beams.partition(line.charAt(_) == '^')
      (unsplit ++ split.flatMap(pos => Set(pos - 1, pos + 1)), splitCount + split.size)
  })._2

val part2 = input.tail.foldLeft[Map[Int, Long]](Map(input.head.indexOf('S') -> 1L))((soFar, line) =>
  soFar.toList.flatMap({
      case (pos, count) if line(pos) == '^' => List((pos - 1, count), (pos + 1, count))
      case (pos, count) => List((pos, count))
    })
    .groupMapReduce(_._1)(_._2)(_ + _)
).values.sum