package advent

object Day01 {

  def run(data: List[Int]): Unit = {
    println(s"Day01.part1 = ${part1(data)}")
    println(s"Day01.part2 = ${part2(data)}")
  }

  def part1(data: List[Int]): Int =
    countIncreasing(data)

  def part2(data: List[Int]): Int =
    countIncreasing(data.sliding(3).map(_.sum).toList)

  def countIncreasing(data: List[Int]): Int =
    data.zip(LazyList.continually(data).flatten.drop(1)).filter { (a, b) => b > a }.size

  def readData(file: String): List[Int] =
    io.Source.fromFile(file).getLines.map(_.toInt).toList

  val testData =
    List(
      199,
      200,
      208,
      210,
      200,
      207,
      240,
      269,
      260,
      263
    )
}


