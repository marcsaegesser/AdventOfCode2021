package advent

object Day09 {

  def run(): Unit = {
    val heights = readData(dataFile)
    println(s"Day09.part1 = ${part1(heights)}")
    println(s"Day09.part2 = ${part2(heights)}")
  }

  def part1(heights: HeightMap): Int = {
    findMins(heights).map(heights.at).map(_+1).sum
  }

  def part2(heights: HeightMap): Int =
    findMins(heights)
      .map(findBasin(heights, _))
      .map(_.size)
      .sorted(Ordering[Int].reverse)
      .take(3)
      .reduce(_*_)

  def findBasin(heights: HeightMap, min: Coord) = {
    def helper(basin: Set[Coord], next: Set[Coord]): Set[Coord] = {
      if(next.isEmpty) basin
      else {
        val nextBasin = basin ++ next
        val n = next.map(heights.adjacent).flatten.filter(p => heights.at(p) < 9) -- nextBasin
        helper(nextBasin, n)
      }
    }

    helper(Set.empty[Coord], Set(min))
  }

  def findMins(heights: HeightMap): Seq[Coord] = {
    for {
      r  <- (0 until heights.rows)
      c  <- (0 until heights.cols)
      rc =  Coord(r, c) if isMin(heights, rc)
    } yield rc
  }

  def isMin(heights: HeightMap, pos: Coord): Boolean = {
    val h = heights.at(pos)
    heights.adjacent(pos).map(heights.at).filter(h >= _).isEmpty
  }


  case class HeightMap(map: Vector[Vector[Int]], rows: Int, cols: Int) {
    def at(pos: Coord): Int =
      map(pos.r)(pos.c)

    def adjacent(coord: Coord): List[Coord] =
      coord.adjacencies.filter { case Coord(r, c) => map.isDefinedAt(r) && map(r).isDefinedAt(c) }
  }

  case class Coord(r: Int, c: Int) {
    def adjacencies: List[Coord] =
      List(Coord(r-1, c), Coord(r, c+1), Coord(r+1, c), Coord(r, c-1))
  }

  def readData(f: String): HeightMap =
    parseData(io.Source.fromFile(f).getLines)

  def parseData(lines: Iterator[String]): HeightMap = {
    val map = lines.map(_.toVector.map(_.asDigit)).toVector
    HeightMap(map, map.size, map(0).size)
  }

  def testData = """2199943210
                   |3987894921
                   |9856789892
                   |8767896789
                   |9899965678""".stripMargin.linesIterator

  val dataFile = "data/Day09.txt"
}
