import common.loadPackets

val input = loadPackets(List("day01.txt")).
  map({
    case s"L$left" => -left.toInt
    case s"R$right" => right.toInt
  })
  .scanLeft(100050)((state, diff) => state + diff)

val part1 = input.count(_ % 100 == 0)

def clicks(from: Int, to: Int): Int =
  (to / 100 - from / 100).abs +
    (if from > to && to % 100 == 0 then 1 else 0) +
    (if from > to && from % 100 == 0 then -1 else 0)

val part2 = input
  .sliding(2)
  .map({case List(a, b) => clicks(a, b)})
  .sum
