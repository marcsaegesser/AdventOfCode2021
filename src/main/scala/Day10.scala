package advent

object Day10 {

  def run(): Unit = {
    val lines = readData(dataFile)
    println(s"Day10.part1 = ${part1(lines)}")
    println(s"Day10.part2 = ${part2(lines)}")
  }

  def part1(lines: List[String]): Int =
    findInvalid(lines).map(scores1).sum

  def part2(lines: List[String]): Long = {
    val results = findValid(lines).map(computeScore2).sorted(Ordering[Long])
    results.drop(results.size/2).head
  }

  def computeScore2(completion: List[Char]): Long =
    completion.foldLeft(0L) { (s, c) => s*5 + scores2(c) }

  def findValid(lines: List[String]): Seq[List[Char]] =
    lines.map(checkLine).collect { case Right(c) => c }.toSeq

  def findInvalid(lines: List[String]): Seq[Char] =
    lines.map(checkLine).collect { case Left(c) => c }.toSeq

  def checkLine(line: String): Either[Char, List[Char]] = {
    def helper(remaining: List[Char], search: List[Char]): Either[Char, List[Char]] = {
      remaining match {
        case Nil => Right(search)
        case h :: t if isOpen(h)  => helper(t, pairMap(h) +: search)
        case h :: t if isClose(h) => if(h == search.head) helper(t, search.tail) else Left(h)
        case _                    => throw Exception(s"Invalid input:  $remaining")
      }
    }

    helper(line.toList, List.empty[Char])
  }

  val pairMap = Map('(' -> ')', '[' -> ']', '{' -> '}', '<' -> '>')
  val closeChars = Set(')', ']', '}', '>')
  val scores1 = Map(')' -> 3, ']' -> 57, '}' -> 1197, '>' -> 25137)
  val scores2 = Map(')' -> 1, ']' -> 2, '}' -> 3, '>' -> 4)


  def isOpen(c: Char): Boolean = pairMap.contains(c)
  def isClose(c: Char): Boolean = closeChars.contains(c)

  def readData(f: String): List[String] =
    io.Source.fromFile(f).getLines.toList

  def testData = """[({(<(())[]>[[{[]{<()<>>
                   |[(()[<>])]({[<{<<[]>>(
                   |{([(<{}[<>[]}>{[]{[(<()>
                   |(((({<>}<{<{<>}{[]{[]{}
                   |[[<[([]))<([[{}[[()]]]
                   |[{[{({}]{}}([{[{{{}}([]
                   |{<[[]]>}<{[{[{[]{()[[[]
                   |[<(<(<(<{}))><([]([]()
                   |<{([([[(<>()){}]>(<<{{
                   |<{([{{}}[<[[[<>{}]]]>[]]""".stripMargin.linesIterator.toList

  val dataFile = "data/Day10.txt"
}
