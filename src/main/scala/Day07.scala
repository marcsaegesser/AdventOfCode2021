package advent

object Day07 {

  def run(): Unit = {
    val crabs = readData(dataFile)
    println(s"Day07.part1 = ${part1(crabs)}")
    println(s"Day07.part2 = ${part2(crabs)}")
  }

  def part1(crabs: List[Int]): Int =
    findMin(crabs, identity)._2

  def part2(crabs: List[Int]): Int =
    findMin(crabs, cost2)._2

  // I'm sure there are much more efficient ways, but this was fast enough to not bother finding them

  def findMin(crabs: List[Int], costF: Int => Int): (Int, Int) = {
    (crabs.min to crabs.max).foldLeft((0, Integer.MAX_VALUE)) { case ((p, cost), pos) =>
      val c = moveAllTo(pos, crabs, costF)
      if(c < cost) (pos, c)
      else         (p, cost)
    }
  }

  def moveAllTo(pos: Int, crabs: List[Int], costF: Int => Int): Int = {
    crabs.foldLeft(0) { case (cost, c) =>  cost + costF(Math.abs(c - pos)) }
  }

  def cost2(dist: Int): Int = (1 to dist).sum

  def readData(f: String): List[Int] =
    parseData(io.Source.fromFile(f).getLines.take(1).mkString)

  def parseData(s: String): List[Int] =
    s.split(",").map(_.toInt).toList

  val testData = """16,1,2,0,4,2,7,1,2,14"""

  val dataFile = "data/Day07.txt"
}
