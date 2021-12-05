package advent

object Day05 {

  def run(): Unit = {
    val lines = readData(dataFile)
    println(s"Day05.part1 = ${part1(lines)}")
    println(s"Day05.part2 = ${part2(lines)}")
  }


  // This can almost certainly be solved more efficiently with math,
  // but I was in the mood for brute force.


  def part1(lines: List[LineSegment]): Int =
    lines
      .flatMap(pointsForLineHV)
      .groupBy(identity)
      .map((k, v) => (k, v.size))
      .count((k, v) => v >= 2)

  def part2(lines: List[LineSegment]): Int =
    lines
      .flatMap(pointsForLine)
      .groupBy(identity)
      .map((k, v) => (k, v.size))
      .count((k, v) => v >= 2)


  case class Coord(x: Int, y: Int)

  case class LineSegment(start: Coord, end: Coord)

  def pointsForLineHV(line: LineSegment): List[Coord] =
    if(isHorizontal(line))    range(line.start.x, line.end.x).map(x => Coord(x, line.start.y)).toList
    else if(isVertical(line)) range(line.start.y, line.end.y).map(y => Coord(line.start.x, y)).toList
    else                      List.empty[Coord]

  def pointsForLine(line: LineSegment): List[Coord] =
    if(isHorizontal(line))    range(line.start.x, line.end.x).map(x => Coord(x, line.start.y)).toList
    else if(isVertical(line)) range(line.start.y, line.end.y).map(y => Coord(line.start.x, y)).toList
    else                      range(line.start.x, line.end.x).zip(range(line.start.y, line.end.y)).map(Coord.apply)

  def range(start: Int, end: Int): List[Int] =
    Range.inclusive(start, end, dir(start, end)).toList

  def dir(start: Int, end: Int): Int =
    if(end < start) -1
    else             1

  def isHorizontal(line: LineSegment): Boolean =
    line.start.y == line.end.y

  def isVertical(line: LineSegment): Boolean =
    line.start.x == line.end.x

  def isDiagnal(line: LineSegment): Boolean =
    (line.start.x - line.end.x) == (line.start.y - line.end.y)

  val lineRegex = """(\d+),(\d+)\s*->\s*(\d+),(\d+)""".r

  def parseLineSegment(s: String): LineSegment = {
    val lineRegex(a, b, c, d) = s
    LineSegment(Coord(a.toInt, b.toInt), Coord(c.toInt, d.toInt))
  }

  def parseData(lines: Iterator[String]): List[LineSegment] =
    lines.map(parseLineSegment).toList

  def readData(f: String): List[LineSegment] =
    parseData(io.Source.fromFile(f).getLines)

  def testData = """0,9 -> 5,9
                   |8,0 -> 0,8
                   |9,4 -> 3,4
                   |2,2 -> 2,1
                   |7,0 -> 7,4
                   |6,4 -> 2,0
                   |0,9 -> 2,9
                   |3,4 -> 1,4
                   |0,0 -> 8,8
                   |5,5 -> 8,2""".stripMargin.linesIterator

  val dataFile = "data/Day05.txt"
}
