package advent

object Day11 {

  def run(): Unit = {
    val map = readData(dataFile)
    println(s"Day11.part1 = ${part1(map)}")
    println(s"Day11.part2 = ${part2(map)}")
  }

  def part1(map: EnergyMap): Int =
    stepN(map, 100)._2

  def part2(map: EnergyMap): Int =
    findSync(map)

  def findSync(map: EnergyMap): Int = {
    def helper(m: EnergyMap, n: Int): (EnergyMap, Int) = {
      if(m.exists((_, e) => e != 0)) helper(step(m)._1, n+1)
      else                           (m, n)
    }

    helper(map, 0)._2
  }

  def stepN(map: EnergyMap, numSteps: Int): (EnergyMap, Int) = {
    def helper(m: EnergyMap, flashes: Int, n: Int): (EnergyMap, Int) =
      if(n == 0) (m, flashes)
      else {
        val (next, fs) = step(m)
        helper(next, flashes + fs, n-1)
      }

    helper(map, 0, numSteps)
  }


  def step(map: EnergyMap): (EnergyMap, Int) = {
    def flash(flashed: Set[Coord], unflashed: Map[Coord, Int]): (EnergyMap, Int) = {
      val (ready, notReady) = unflashed.partition(_._2 > 9 )
      if(ready.isEmpty) ((flashed.map((_, 0)).toMap ++ notReady), flashed.size)
      else {
        val next = ready.flatMap((c, _) => c.adjacencies).toList.filter(notReady.keySet.contains)
        val n = next.foldLeft(notReady) { case (m, c) => m.updatedWith(c)(_.map(_+1)) }
        flash(flashed ++ ready.keySet, n)
      }
    }

    flash(Set.empty[Coord], map.map((k, v) => (k, v+1)))
  }

  def show(map: EnergyMap) =
    map.toList
      .groupBy((c, _) => c.r)
      .map { case (r, v) => (r, v.sortBy(_._1.c).map(_._2).mkString) }
      .toList
      .sortBy(_._1)
      .map(_._2)
      .mkString("\n", "\n", "")

  type EnergyMap = Map[Coord, Int]

  case class Coord(r: Int, c: Int) {
    def adjacencies: List[Coord] =
      List(
        Coord(r-1, c),   // N
        Coord(r-1, c+1), // NE
        Coord(r,   c+1), // E
        Coord(r+1, c+1), // SE
        Coord(r+1, c),   // S
        Coord(r+1, c-1), // SW
        Coord(r,   c-1), // W
        Coord(r-1, c-1)) // NW
  }

  def readData(f: String): EnergyMap =
    parseData(io.Source.fromFile(f).getLines)

  def parseData(lines: Iterator[String]): EnergyMap = {
    def parseLine(line: String, r: Int) =
      line.zipWithIndex.map((e, c) => (Coord(r, c) -> e.asDigit))

    lines.zipWithIndex.flatMap((r, l) => parseLine(r, l)).toMap
  }


  def testData = """5483143223
                   |2745854711
                   |5264556173
                   |6141336146
                   |6357385478
                   |4167524645
                   |2176841721
                   |6882881134
                   |4846848554
                   |5283751526""".stripMargin.linesIterator

  def testDataSmall = """11111
                        |19991
                        |19191
                        |19991
                        |11111""".stripMargin.linesIterator

  val dataFile = "data/Day11.txt"
}
