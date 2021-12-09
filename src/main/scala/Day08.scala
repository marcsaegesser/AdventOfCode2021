package advent

object Day08 {

  def run(): Unit = {
    val entries = readData(dataFile)
    println(s"Day08.part1 = ${part1(entries)}")
    println(s"Day08.part2 = ${part2(entries)}")
  }

  def part1(entries: List[Entry]): Int = {
    entries.map(e => findEasy(e.result).size).sum
  }

  def part2(entries: List[Entry]): Int = {
    entries.foldLeft(List.empty[Int]) { (accum, e) =>
      decodeResult(e.result, findMapping(e.testDigits)) +: accum
    }.sum
  }

  def findEasy(digits: List[Digit]): List[Digit] =
    digits.filter(d => isOne(d) || isFour(d) || isSeven(d) || isEight(d))

  type SegmentMap = Map[Char, Set[Char]]

  def decodeResult(result: List[Digit], m: Map[Set[Char], Int]): Int =
    result.flatMap(m.get).mkString.toInt

  def findMapping(digits: List[Digit]): Map[Set[Char], Int] = {
    val c1 = digits.find(_.size == 2).get
    val c4 = digits.find(_.size == 4).get
    val c7 = digits.find(_.size == 3).get
    val c8 = digits.find(_.size == 7).get
    val a = c7 -- c1
    val g = digits.map( d => (d -- (c4 ++ a))).find(_.size == 1).get
    val e = digits.map( d => (d -- (c4 ++ a))).find(_.size == 2).map(_ -- g).get
    val c9 = c4 ++ a ++ g
    val acfge = c7 ++ e ++ g
    val bd = digits.map(d => d -- acfge).filter(_.size == 2).distinct.head
    val b = digits.map(d => d -- acfge).filter(_.size == 1).groupBy(identity).find((k, v) => v.size == 1).get._1
    val d = bd -- b
    val c0 = c7 ++ b ++ e ++ g
    val c3 = c7 ++ d ++ g
    val abdeg = a ++ b ++ d ++ e ++ g
    val cf = digits.map(d => d -- abdeg).filter(_.size == 2).distinct.head
    val c = digits.map(d => d -- abdeg).filter(_.size == 1).groupBy(identity).find((k, v) => v.size == 1).get._1
    val f = cf -- c
    val c2 = a ++ c ++ d ++ e ++ g
    val c5 = a ++ b ++ d ++ f ++ g
    val c6 = a ++ b ++ d ++ e ++ f ++ g

    Map(c0 -> 0, c1 -> 1, c2 -> 2, c3 -> 3, c4 -> 4, c5 -> 5, c6 -> 6, c7 -> 7, c8 -> 8, c9 -> 9)
  }

  def isOne(digit: Digit): Boolean   = digit.size == 2
  def isFour(digit: Digit): Boolean  = digit.size == 4
  def isSeven(digit: Digit): Boolean = digit.size == 3
  def isEight(digit: Digit): Boolean = digit.size == 7

  type Digit = Set[Char]
  case class Entry(testDigits: List[Digit], result: List[Digit])

  def parseEntry(line: String): Entry = {
    line.split('|') match {
      case Array(ds, rs) => Entry(ds.trim.split(" ").map(_.toSet).toList, rs.trim.split(" ").map(_.toSet).toList)
    }
  }

  def readData(f: String): List[Entry] =
    parseData(io.Source.fromFile(f).getLines)

  def parseData(lines: Iterator[String]): List[Entry] =
    lines.map(parseEntry).toList

  def testData = """be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
                   |edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
                   |fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
                   |fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
                   |aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
                   |fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
                   |dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
                   |bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
                   |egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
                   |gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce""".stripMargin.linesIterator

  val dataFile = "data/Day08.txt"
}
