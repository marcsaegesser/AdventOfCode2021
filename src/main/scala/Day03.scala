package advent

object Day03 {

  def run(): Unit = {
    val data = readData("data/Day03.txt")
    println(s"Day03.part1 = ${part1(data)}")
    println(s"Day03.part2 = ${part2(data)}")
  }

  def part1(data: List[Vector[Int]]): Long = {
    val (g, e) = computeGammaEpsilon(data)
    java.lang.Long.parseLong(g.mkString, 2) * java.lang.Long.parseLong(e.mkString, 2)
  }

  def part2(data: List[Vector[Int]]) = {
    def helper(remaining: List[Vector[Int]], f: (List[Vector[Int]], Int) => Int, i: Int): Vector[Int] =
      remaining match {
        case r :: Nil => r
        case rs       => helper(rs.filter(_(i) == f(remaining, i)), f, i+1)
      }

    val oxy = java.lang.Long.parseLong(helper(data, gamma, 0).mkString, 2)
    val c02 = java.lang.Long.parseLong(helper(data, epsilon, 0).mkString, 2)

    oxy * c02
  }

  // For part 1 compute gamma and epsilon for all the data
  def computeGammaEpsilon(data: List[Vector[Int]]) = {
    data
      .transpose
      .map(_.partition(_ == 0))
      .map((a, b) => if(a.size > b.size) (0, 1) else (1, 0))
      .unzip
  }

  // For part2 gamma and epsilon need to be computed for each iteration and we only care about one bit at a time
  def gamma(data: List[Vector[Int]], i: Int) = {
    val (zs, os) = data.map(_(i)).partition(_ == 0)
    if(zs.size > os.size) 0 else 1
  }

  def epsilon(data: List[Vector[Int]], i: Int) = {
    val (zs, os) = data.map(_(i)).partition(_ == 0)
    if(zs.size <= os.size) 0 else 1
  }

  def readData(f: String): List[Vector[Int]] =
    parseData(io.Source.fromFile(f).getLines)

  def parseData(lines: Iterator[String]): List[Vector[Int]] =
    lines.map(_.toVector.map(_.asDigit)).toList

  val testData = """00100
                   |11110
                   |10110
                   |10111
                   |10101
                   |01111
                   |00111
                   |11100
                   |10000
                   |11001
                   |00010
                   |01010""".stripMargin.linesIterator
}
