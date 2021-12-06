package advent

object Day06 {

  def run(): Unit = {
    val fish = readData(dataFile)
    println(s"Day06.part1 = ${part1(fish)}")
    println(s"Day06.part2 = ${part2(fish)}")
  }

  def part1(fish: State): Long =
    runN(80, fish).values.sum

  def part2(fish: State): Long =
    runN(256, fish).values.sum

  def runN(n: Int, fish: State): State = {
    if(n == 0) fish
    else       runN(n-1, step(fish))
  }

  def step(state: State): State = {
    state.foldLeft(Map.empty[Age, Long]) { case (s, (a, n)) =>
      a match {
        case 0 => s.updatedWith(6)(_.map(_+n).orElse(Some(n))).updated(8, n)
        case _ => s.updatedWith(a-1)(_.map(_+n).orElse(Some(n)))
      }
    }
  }


  type Age   = Long
  type State = Map[Age, Long]

  def readData(f: String): State =
    parseData(io.Source.fromFile(f).getLines.take(1).mkString)

  def parseData(data: String): State =
    data.split(",")
      .map(_.toLong)
      .groupBy(identity)
      .map((k, v) => (k, v.size.toLong))

  val testData = """3,4,3,1,2"""

  val dataFile = "data/Day06.text"
}
