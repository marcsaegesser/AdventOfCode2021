package advent

object Day20 {

  def run(): Unit = {
    val state = readData(dataFile)
    println(s"Day20.part1 = ${part1(state)}")
    println(s"Day20.part2 = ${part2(state)}")
  }

  def part1(state: State): Int = {
    applyN(state, 2).image.size
  }

  def part2(state: State): Int =
    applyN(state, 50).image.size

  case class Coord(r: Int, c: Int)
  case class State(image: Set[Coord], region: (Coord, Coord), outside: Boolean, algo: Set[Int])

  def applyN(state: State, n: Int): State =
    if(n == 0) state
    else       applyN(applyAlgorithm(state), n-1)

  def applyAlgorithm(state: State): State = {
    val nextImage =
      (for {
        r <- state.region._1.r-1 to state.region._2.r+1
        c <- state.region._1.c-1 to state.region._2.c+1
        v =  valueForWindow(Coord(r, c), state)
        p =  Coord(r, c) if state.algo.contains(v)
      } yield p).toSet

    val nextOutside =
      if(state.outside) state.algo.contains(511)
      else              state.algo.contains(0)

    State(nextImage, region(nextImage), nextOutside, state.algo)
  }

  def window(c: Coord): List[Coord] =
    List(
      Coord(c.r-1, c.c-1),
      Coord(c.r-1, c.c),
      Coord(c.r-1, c.c+1),
      Coord(c.r,   c.c-1),
      Coord(c.r,   c.c),
      Coord(c.r,   c.c+1),
      Coord(c.r+1, c.c-1),
      Coord(c.r+1, c.c),
      Coord(c.r+1, c.c+1)
    )

  def region(image: Set[Coord]): (Coord, Coord) =
    (Coord(image.map(_.r).min, image.map(_.c).min),
      Coord(image.map(_.r).max, image.map(_.c).max)
    )

  def inRegion(pos: Coord, region: (Coord, Coord)): Boolean =
    pos.r >= region._1.r && pos.r <= region._2.r && pos.c >= region._1.c && pos.c <= region._2.c

  def isLit(pos: Coord, state: State): Boolean =
    if(inRegion(pos, state.region)) state.image.contains(pos)
    else                            state.outside

  def valueForWindow(pos: Coord, state: State): Int = {
    Integer.parseInt(window(pos).map(c => if(isLit(c, state)) "1" else "0").mkString, 2)

  }

  def parseAlgorithm(s: String): Set[Int] =
    s.zipWithIndex.foldLeft(Set.empty[Int]) { case (a, (c, i)) => if(c=='#') a + i else a }

  def parseImage(lines: Iterator[String]): Set[Coord] = {
    def parseRow(r: Int, s: String): Set[Coord] =
      s.zipWithIndex.foldLeft(Set.empty[Coord]) { case (a, (c, col)) => if(c=='#') a + Coord(r, col) else a }

    lines.zipWithIndex.foldLeft(Set.empty[Coord]) { case (a, (l, r)) => a ++ parseRow(r, l) }
  }

  def readData(f: String): State =
    parseData(io.Source.fromFile(f).getLines())

  def parseData(lines: Iterator[String]): State = {
    val algo = parseAlgorithm(lines.take(1).mkString)
    val image = parseImage(lines.drop(1))
    State(image, region(image), false, algo)
  }

  def testData = """..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#
                   |
                   |#..#.
                   |#....
                   |##..#
                   |..#..
                   |..###""".stripMargin.linesIterator

  val dataFile = "data/Day20.txt"
}
