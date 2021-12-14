package advent

object Day14 {

  def run(): Unit = {
    val (template, mapping) = readData(dataFile)
    println(s"Day14.part1 = ${part1(template, mapping)}")
    println(s"Day14.part2 = ${part2(template, mapping)}")
  }

  type Pair = (Char, Char)

  def part1(template: String, mapping: Map[Pair, Char]): Long = {
    val result = stepN(pairFreqs(template), mapping, 10)
    val freqs = charFreqs(template, result).values.toSet
    freqs.max - freqs.min
  }

  def part2(template: String, mapping: Map[Pair, Char]): Long = {
    val result = stepN(pairFreqs(template), mapping, 40)
    val freqs = charFreqs(template, result).values.toSet
    freqs.max - freqs.min
  }

  def pairFreqs(template: String): Map[Pair, Long] =
    template.sliding(2, 1)
      .toList
      .map(p => (p(0), p(1)))
      .groupBy(identity).map((k, v) => (k, v.size.toLong))

  def charFreqs(template: String, pairFreqs: Map[Pair, Long]): Map[Char, Long] =
    pairFreqs.foldLeft(Map.empty[Char, Long]){ case (m, (k, v)) =>
      m.updatedWith(k.head)(_.map(_+v).orElse(Some(v)))
    }.updatedWith(template.last)(_.map(_+1).orElse(Some(1)))

  def stepN(freqs: Map[Pair, Long], mapping: Map[Pair, Char], n: Int): Map[Pair, Long] =
    if(n == 0) freqs
    else       stepN(step(freqs, mapping), mapping, n-1)

  def step(freq: Map[Pair, Long], mapping: Map[Pair, Char]): Map[Pair, Long] = {
    freq.foldLeft(Map.empty[Pair, Long]) { case (fs, (p, n)) =>
      val c = mapping(p)
      fs
        .updatedWith((p._1, c))(_.map(_+n).orElse(Some(n)))
        .updatedWith((c, p._2))(_.map(_+n).orElse(Some(n)))
    }
  }

  val templateRegex = """(\w+)""".r
  val pairRegex = """(\w)(\w)\s*->\s*(\w)""".r

  def readData(f: String): (String, Map[Pair, Char]) =
    parseData(io.Source.fromFile(f).getLines)

  def parseData(lines: Iterator[String]): (String, Map[Pair, Char]) = {
    lines.foldLeft(("", Map.empty[Pair, Char])) { case ((t, m), l) =>
      l match {
        case templateRegex(s) => (s, m)
        case pairRegex(a, b, c)  => (t, m.updated((a.head, b.head), c.head))
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
