package advent

object Day13 {

  def run(): Unit = {
    val page = readData(dataFile)
    println(s"Day13.part1 = ${part1(page)}")
    println(s"Day13.part2 = ${part2(page)}")
  }

  def part1(page: Page): Int =
    applyFold(page.points, page.folds.head).size

  def part2(page: Page): String =
    show(applyFolds(page))

  def applyFolds(page: Page): Set[Coord] =
    page.folds.foldLeft(page.points)(applyFold)

  def applyFold(points: Set[Coord], fold: Fold): Set[Coord] =
    fold match {
      case Fold.Horizontal(fy) => points.map { case Coord(x, y) => if(y <= fy) Coord(x, y) else Coord(x, 2*fy-y) }
      case Fold.Vertical(fx)   => points.map { case Coord(x, y) => if(x <= fx) Coord(x, y) else Coord(2*fx-x, y) }
    }

  case class Page(points: Set[Coord], folds: List[Fold])

  def show(points: Set[Coord]) = {
    import Math._
    val (mx, my) = points.foldLeft((0, 0)) { case ((mx, my), Coord(x, y)) => (max(mx, x), max(my, y)) }
    def showLine(y: Int): String = (0 to mx).map(x => if(points.contains(Coord(x, y))) "#" else ".").mkString
    (0 to my).map(y => showLine(y)).mkString("\n", "\n", "")
  }

  case class Coord(x: Int, y: Int)
  enum Fold {
    case Horizontal(y: Int)
    case Vertical(x: Int)
  }

  def parsePoint(line: String): Coord =
    line.split(",") match {
      case Array(x, y) => Coord(x.toInt, y.toInt)
    }

  val foldRegex = """fold along (\w)=(\d+)""".r
  def parseFold(line: String): Fold = {
    line match {
      case foldRegex("y", c) => Fold.Horizontal(c.toInt)
      case foldRegex("x", c) => Fold.Vertical(c.toInt)
    }
  }

  def readData(f: String): Page = {
    parseData(io.Source.fromFile(f).getLines)
  }

  def parseData(lines: Iterator[String]): Page = {
    val points = lines.takeWhile(!_.isEmpty).map(parsePoint).toSet
    val folds = lines.dropWhile(!_.startsWith("f")).map(parseFold).toList
    Page(points, folds)
  }

  def testData = """6,10
                   |0,14
                   |9,10
                   |0,3
                   |10,4
                   |4,11
                   |6,0
                   |6,12
                   |4,1
                   |0,13
                   |10,12
                   |3,4
                   |3,0
                   |8,4
                   |1,10
                   |2,14
                   |8,10
                   |9,0
                   |
                   |fold along y=7
                   |fold along x=5""".stripMargin.linesIterator

  val dataFile = "data/Day13.txt"
}
