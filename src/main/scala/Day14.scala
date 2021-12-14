package advent

object Day14 {

  def run(): Unit = {
    val (template, mapping) = readData(dataFile)
    println(s"Day14.part1 = ${part1(template, mapping)}")
    println(s"Day14.part2 = ${part2(template, mapping)}")
  }

  def part1(template: String, mapping: Map[String, String]): Long = {
    val result = stepN(pairFreqs(template), mapping, 10)
    val keys = charFreqs(template, result).map((k, v) => (v, k)).keySet
    keys.max - keys.min
  }

  def part2(template: String, mapping: Map[String, String]): Long = {
    val result = stepN(pairFreqs(template), mapping, 40)
    val keys = charFreqs(template, result).map((k, v) => (v, k)).keySet
    keys.max - keys.min
  }

  def pairFreqs(template: String): Map[String, Long] =
    template.sliding(2, 1)
      .toList
      .groupBy(identity).map((k, v) => (k, v.size.toLong))

  def charFreqs(template: String, pairFreqs: Map[String, Long]): Map[Char, Long] =
    pairFreqs.foldLeft(Map.empty[Char, Long]){ case (m, (k, v)) =>
      m.updatedWith(k.head)(_.map(_+v).orElse(Some(v)))
    }.updatedWith(template.last)(_.map(_+1).orElse(Some(1)))

  def stepN(freqs: Map[String, Long], mapping: Map[String, String], n: Int): Map[String, Long] =
    if(n == 0) freqs
    else       stepN(step(freqs, mapping), mapping, n-1)

  def step(freq: Map[String, Long], mapping: Map[String, String]): Map[String, Long] = {
    freq.foldLeft(Map.empty[String, Long]) { case (fs, (p, n)) =>
      val c = mapping(p)
      val p1 = s"${p(0)}$c"
      val p2 = s"$c${p(1)}"
      fs
        .updatedWith(p1)(_.map(_+n).orElse(Some(n)))
        .updatedWith(p2)(_.map(_+n).orElse(Some(n)))
    }
  }

  val templateRegex = """(\w+)""".r
  val pairRegex = """(\w+)\s*->\s*(\w+)""".r

  def readData(f: String): (String, Map[String, String]) =
    parseData(io.Source.fromFile(f).getLines)

  def parseData(lines: Iterator[String]): (String, Map[String, String]) = {
    lines.foldLeft(("", Map.empty[String, String])) { case ((t, m), l) =>
      l match {
        case templateRegex(s) => (s, m)
        case pairRegex(a, b)  => (t, m.updated(a, b))
        case _                => (t, m)
      }
    }
  }

  def testData = """NNCB
                   |
                   |CH -> B
                   |HH -> N
                   |CB -> H
                   |NH -> C
                   |HB -> C
                   |HC -> B
                   |HN -> C
                   |NN -> C
                   |BH -> H
                   |NC -> B
                   |NB -> B
                   |BN -> B
                   |BB -> N
                   |BC -> B
                   |CC -> N
                   |CN -> C""".stripMargin.linesIterator

  val dataFile = "data/Day14.txt"
}
