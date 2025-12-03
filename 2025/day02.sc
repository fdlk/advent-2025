import common.loadPackets

val input = loadPackets(List("day02.txt")).head.split(",")
  .map({case s"${from}-${to}" => (from.toLong, to.toLong)})

def isInvalid(id: Long): Boolean =
  val string = id.toString
  if (string.length % 2 == 1)
    false
  else
    val left = string.substring(0, string.length / 2)
    val right = string.substring(string.length / 2)
    left == right

val part1 = input.flatMap({
  case (start, end) => (start to end).filter(isInvalid)
}).sum

def isInvalidPart2(id: Long): Boolean =
  val string = id.toString
  (1 to string.length / 2).exists(size => {
    string.length % size == 0 &&
    string.substring(0, size).repeat(string.length / size) == string
  })

val part2 = input.flatMap({
  case (start, end) => (start to end).filter(isInvalidPart2)
}).sum