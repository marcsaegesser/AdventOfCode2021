package advent

import Math._

object Day19 {

  def mergeScanners(scanners: List[Scanner]): Set[Coord] = {
    def helper(accum: Set[Coord], remaining: List[Scanner]) : Set[Coord] = {
      if(remaining.isEmpty) accum
      else {
        val (m, r) = remaining.map(s => (s, scannerOffset(accum, s))).partition(_._2.isDefined)
        println(s"$m")
        println(s"$r")
        val nextAccum =
          m.foldLeft(accum) {
            case (a, (s, Some(xo, yo, zo))) => s.map { case Coord(x, y, z) => Coord(x+xo, y+yo, z+zo)} ++ a
            case _ => accum // Can't happen
          }
        helper(nextAccum, r.map(_._1))
      }
    }


    helper(scanners.head, scanners.tail)
  }

  def scannerOffset(s1: Set[Coord], s2: Set[Coord])/*: Option[(Int, Int, Int)]*/ = {
    val s1Offsets = s1.map(c1 => (c1, s1.filterNot(_ == c1).map(c2 => (c2, offsetSig(diff(c1, c2))))))
    val s2Offsets = s2.map(c1 => (c1, s2.filterNot(_ == c1).map(c2 => (c2, offsetSig(diff(c1, c2))))))

    val matches =
      s1Offsets.map { (c1, os1) =>
        val s = os1.map(_._2)//.toSet                // The set of s1's offsets
        val x =
          s2Offsets.map { case (c2, os2) =>
            (c2, os2.filter((_, o) => s.contains(o))) // For each coord in s2, the offsets that match c1's offsets
          }.maxBy(_._2.size)                          // The longest list of matching offsets
        (c1, x)
      }.filterNot(_._2._2.isEmpty)
        .map { case (c1, (c2, ms)) => (c1, c2) }

    if(matches.size < 12) None
    else {
      val xOffset = determineOffset(matches.map ((c1, c2) => (c1.x, c2.x)))
      val yOffset = determineOffset(matches.map ((c1, c2) => (c1.y, c2.y)))
      val zOffset = determineOffset(matches.map ((c1, c2) => (c1.z, c2.z)))

      Some((xOffset, yOffset, zOffset))
    }
  }

  def fubar(s1: Set[Coord], s2: Set[Coord]) = {
    val s1Offsets = s1.map(c1 => (c1, s1.filterNot(_ == c1).map(c2 => (c2, diff(c1, c2)))))
    val s2Offsets = s2.map(c1 => (c1, s2.filterNot(_ == c1).map(c2 => (c2, diff(c1, c2)))))

    val matches =
      s1Offsets.map { (c1, os1) =>
        val s = os1.map((_, d) => offsetSig(d))                         // The set of s1's offset signatures
        val x =
          s2Offsets.map { case (c2, os2) =>
            val s2 = os2.map((_, d) => offsetSig(d))
            (c2, os1.filter((_, o) => s2.contains(offsetSig(o))), os2.filter((_, o) => s.contains(offsetSig(o)))) // For each coord in s2, the offsets that match c1's offsets
          }.maxBy(_._3.size)                          // The longest list of matching offsets
        (c1, x)
      }.filterNot(_._2._2.isEmpty)
        .map { case (c1, (c2, ms1, ms2)) => (c1, ms1, c2, ms2) }

    matches
  }

  def determineOffset(input: Set[(Int, Int)]): Int = {
    val a = input.map((a, b) => a+b)
    if(a.size == 1) a.head
    else input.map((a, b) => a-b).toSet.head
  }

  case class Offset(x: Int, y: Int, z: Int)
  case class Coord(x: Int, y: Int, z: Int)
  // case class Beacon(id: Int, coord: Coord)
  // case class Scanner(id: Int, coords: List[Coord])
  type Scanner = Set[Coord]

  def diff(c1: Coord, c2: Coord): Offset =
    Offset(c1.x - c2.x, c1.y - c2.y, c1.z - c2.z)

  def coordsMatch(c1: Coord, c2: Coord): Boolean = {
    Set(abs(c1.x), abs(c1.y), abs(c2.z)) == Set(abs(c2.x), abs(c2.y), abs(c2.z))
  }

  def dist(c1: Coord, c2: Coord): Int =
    abs(c1.x - c2.x) + abs(c1.y - c2.y) + abs(c1.z - c2.z)

  def coordSig(c: Coord): Set[Int] =
    Set(abs(c.x), abs(c.y), abs(c.z))

  def offsetSig(c: Offset): Set[Int] =
    Set(abs(c.x), abs(c.y), abs(c.z))

  val idRegex = """--- scanner (\d+) ---""".r
  val coordRegex = """(-?\d+),(-?\d+),(-?\d+)""".r

  def parseScanner(lines: List[String]): Scanner = {
    val idRegex(id) = lines.head
    val coords = lines.tail.map { l =>
      val coordRegex(x, y , z) = l
      Coord(x.toInt, y.toInt, z.toInt)
    }

    coords.toSet
    // Scanner(id.toInt, coords)
  }

  def parseScanners(lines: Iterator[String]): List[Scanner] = {
    def helper(accum: List[Scanner]): List[Scanner] =
      if(lines.isEmpty) accum.reverse
      else helper(parseScanner(lines.takeWhile(!_.isEmpty).toList) +: accum)

    helper(List.empty)
  }

  def testData = """--- scanner 0 ---
                   |404,-588,-901
                   |528,-643,409
                   |-838,591,734
                   |390,-675,-793
                   |-537,-823,-458
                   |-485,-357,347
                   |-345,-311,381
                   |-661,-816,-575
                   |-876,649,763
                   |-618,-824,-621
                   |553,345,-567
                   |474,580,667
                   |-447,-329,318
                   |-584,868,-557
                   |544,-627,-890
                   |564,392,-477
                   |455,729,728
                   |-892,524,684
                   |-689,845,-530
                   |423,-701,434
                   |7,-33,-71
                   |630,319,-379
                   |443,580,662
                   |-789,900,-551
                   |459,-707,401
                   |
                   |--- scanner 1 ---
                   |686,422,578
                   |605,423,415
                   |515,917,-361
                   |-336,658,858
                   |95,138,22
                   |-476,619,847
                   |-340,-569,-846
                   |567,-361,727
                   |-460,603,-452
                   |669,-402,600
                   |729,430,532
                   |-500,-761,534
                   |-322,571,750
                   |-466,-666,-811
                   |-429,-592,574
                   |-355,545,-477
                   |703,-491,-529
                   |-328,-685,520
                   |413,935,-424
                   |-391,539,-444
                   |586,-435,557
                   |-364,-763,-893
                   |807,-499,-711
                   |755,-354,-619
                   |553,889,-390
                   |
                   |--- scanner 2 ---
                   |649,640,665
                   |682,-795,504
                   |-784,533,-524
                   |-644,584,-595
                   |-588,-843,648
                   |-30,6,44
                   |-674,560,763
                   |500,723,-460
                   |609,671,-379
                   |-555,-800,653
                   |-675,-892,-343
                   |697,-426,-610
                   |578,704,681
                   |493,664,-388
                   |-671,-858,530
                   |-667,343,800
                   |571,-461,-707
                   |-138,-166,112
                   |-889,563,-600
                   |646,-828,498
                   |640,759,510
                   |-630,509,768
                   |-681,-892,-333
                   |673,-379,-804
                   |-742,-814,-386
                   |577,-820,562
                   |
                   |--- scanner 3 ---
                   |-589,542,597
                   |605,-692,669
                   |-500,565,-823
                   |-660,373,557
                   |-458,-679,-417
                   |-488,449,543
                   |-626,468,-788
                   |338,-750,-386
                   |528,-832,-391
                   |562,-778,733
                   |-938,-730,414
                   |543,643,-506
                   |-524,371,-870
                   |407,773,750
                   |-104,29,83
                   |378,-903,-323
                   |-778,-728,485
                   |426,699,580
                   |-438,-605,-362
                   |-469,-447,-387
                   |509,732,623
                   |647,635,-688
                   |-868,-804,481
                   |614,-800,639
                   |595,780,-596
                   |
                   |--- scanner 4 ---
                   |727,592,562
                   |-293,-554,779
                   |441,611,-461
                   |-714,465,-776
                   |-743,427,-804
                   |-660,-479,-426
                   |832,-632,460
                   |927,-485,-438
                   |408,393,-506
                   |466,436,-512
                   |110,16,151
                   |-258,-428,682
                   |-393,719,612
                   |-211,-452,876
                   |808,-476,-593
                   |-575,615,604
                   |-485,667,467
                   |-680,325,-822
                   |-627,-443,-432
                   |872,-547,-609
                   |833,512,582
                   |807,604,487
                   |839,-516,451
                   |891,-625,532
                   |-652,-548,-490
                   |30,-46,-14""".stripMargin.linesIterator
}
