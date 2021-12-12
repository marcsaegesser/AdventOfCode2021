package advent

object Day12 {

  def run(): Unit = {
    val map = readData(dataFile)
    println(s"Day12.part1 = ${part1(map)}")
    println(s"Day12.part2 = ${part2(map)}")
  }

  def part1(map: CaveMap): Int = {
    def helper(accum: List[Path], path: Path, visited: Set[Cave]): List[Path] = {
      path match {
        case "end" :: _ => path.reverse +: accum
        case h :: _ =>
          val next = map(h) -- visited
          val v = if(h.head.isLower) visited + h else visited
          next.foldLeft(accum) { case (a, n) =>
            helper(a, n +: path, v)
          }
        case Nil => ???  // Bug
      }
    }

    helper(List.empty[Path], List("start"), Set.empty[Cave]).size
  }

  def part2(map: CaveMap): Int = {
    def helper(accum: Set[Path], path: Path, visited: Set[Cave], twice: Option[Cave]): Set[Path] =
      path match {
        case "end" :: _ => accum + path.reverse
        case h :: _ if Some(h) == twice =>
          val next = map(h) -- visited
          next.foldLeft(accum) { case (a, n) =>
            helper(a, n +: path, visited, None)
          }
        case h :: _ =>
          val next = map(h) -- visited
          val v = if(h.head.isLower) visited + h else visited
          next.foldLeft(accum) { case (a, n) =>
            helper(a, n +: path, v, twice)
          }
        case Nil => ???  // Bug
      }

    val littles = map.keySet.filter(_.head.isLower) -- Set("start", "end")
    littles.foldLeft(Set.empty[Path]) { case (a, l) => a ++ helper(Set.empty[Path], List("start"), Set.empty[Cave], Some(l)) }.size
  }



  type Cave    = String
  type CaveMap = Map[Cave, Set[Cave]]
  type Path    = List[Cave]

  def readData(f: String): CaveMap =
    parseData(io.Source.fromFile(f).getLines)

  def parseData(lines: Iterator[String]): CaveMap =
    lines.toList
      .map(_.split("-"))
      .flatMap { case Array(f, t) => List((f, t), (t, f)) }
      .groupBy(_._1)
      .map((k, v) => (k, v.map(_._2).toSet))

  def testData1 = """start-A
                    |start-b
                    |A-c
                    |A-b
                    |b-d
                    |A-end
                    |b-end""".stripMargin.linesIterator

  def testData2 = """dc-end
                    |HN-start
                    |start-kj
                    |dc-start
                    |dc-HN
                    |LN-dc
                    |HN-end
                    |kj-sa
                    |kj-HN
                    |kj-dc""".stripMargin.linesIterator

  def testData3 = """fs-end
                    |he-DX
                    |fs-he
                    |start-DX
                    |pj-DX
                    |end-zg
                    |zg-sl
                    |zg-pj
                    |pj-he
                    |RW-he
                    |fs-DX
                    |pj-RW
                    |zg-RW
                    |start-pj
                    |he-WI
                    |zg-he
                    |pj-fs
                    |start-RW""".stripMargin.linesIterator

  val dataFile = "data/Day12.txt"
}
