package advent

// Very ugly, but functional
object Day18 {

  def run(): Unit = {
    val ns = readData(dataFile)
    println(s"Day18.part1 = ${part1(ns)}")
    println(s"Day18.part2 = ${part2(ns)}")
  }

  def part1(ns: List[SnailfishNumber]): Int = {
    magnitude(addList(ns))
  }

  def part2(ns: List[SnailfishNumber]): Int = {
    ns.combinations(2)
      .collect { case List (n1, n2) => List(magnitude(addSnail(n1, n2)), magnitude(addSnail(n2, n1))) }
      .flatten
      .max
  }

  type SnailfishNumber = String

  def addList(ns: List[SnailfishNumber]): SnailfishNumber =
    ns.reduce(addSnail)

  def addSnail(a: SnailfishNumber, b: SnailfishNumber): SnailfishNumber =
    reduce(s"[$a,$b]")

  def reduce(n: SnailfishNumber): SnailfishNumber = {
    def step(current: SnailfishNumber): SnailfishNumber = {
      val nextE = tryExplode(current)
      if(nextE != current) step(nextE)
      else {
        val nextS = trySplit(current)
        if(nextS != current) step(nextS)
        else current
      }
    }

    step(n)
  }

  case class State(pos: Int, depth: Int, stack: List[Int], numIndex: Int, numbers: Vector[NumberEntry])

  def initialState(numbers: Vector[NumberEntry]) =
    State(0, 0, List.empty[Int], -1, numbers)

  def addToNumberAt(s: String, i: Int, toAdd: Int, numbers: Vector[NumberEntry]): String = {
    if(!numbers.isDefinedAt(i)) s
    else {
      val ne = numbers(i)
      val (left, _) = s.splitAt(ne.start)
      val (_, right) = s.splitAt(ne.end)
      left ++ (ne.n + toAdd).toString ++ right
    }
  }

  def doExplode(n: SnailfishNumber, state: State): SnailfishNumber = {
    state match { case (State(pos, depth, stack, numIndex, numbers)) =>
      val List(i2, i1) = stack
      val List(n2, n1) = stack.map(numbers.apply)

      val left = addToNumberAt(n.splitAt(n1.start-1)._1, i1-1, n1.n, numbers)
      val right = addToNumberAt(n, i2+1, n2.n, numbers).splitAt(n2.end+1)._2

      left ++ "0" ++ right
    }
  }

  def tryExplode(n: SnailfishNumber): SnailfishNumber = {
    def run(state: State): SnailfishNumber = {
      state match { case State(pos, depth, stack, numIndex, numbers) =>
        if(pos >= n.size) n
        else
          n(pos) match {
            case '[' => run(state.copy(pos=pos+1, depth=depth+1, stack=List.empty[Int]))
            case ']' =>
              if(stack.size == 2 && depth == 5) doExplode(n, state)
              else                run(state.copy(pos=pos+1, depth=depth-1, stack=List.empty[Int]))
            case ',' => run(state.copy(pos=pos+1))
            case d   =>
              val nIdx = numIndex+1
              run(state.copy(pos=numbers(nIdx).end, stack=nIdx +: stack, numIndex=nIdx))
          }
      }
    }

    run(initialState(scanNumbers(n)))
  }

  def magPair(a: Int, b: Int): Int =
    a*3 + b*2

  def doMagnitude(n: SnailfishNumber, state: State): SnailfishNumber = {
    state match { case State(pos, depth, stack, numIndex, numbers) =>
      val List(n2, n1) = stack.map(numbers.apply)
      val mag = magPair(n1.n, n2.n)
      val left = n.splitAt(n1.start-1)._1
      val right = n.splitAt(n2.end+1)._2

      left ++ mag.toString ++ right
    }
  }

  def magnitude(n: SnailfishNumber): Int = {
    def step(current: SnailfishNumber): Int = {
      val numbers = scanNumbers(current)
      if(numbers.size == 2) magPair(numbers(0).n, numbers(1).n)
      else                  step(tryMagnitude(current))
    }

    step(n)
  }

  def tryMagnitude(n: SnailfishNumber): SnailfishNumber = {
    def step(state: State): SnailfishNumber = {
      state match { case State(pos, depth, stack, numIndex, numbers) =>
        if(numbers.size == 2) n
        else
          n(pos) match {
            case '[' => step(state.copy(pos=pos+1, depth=depth+1, stack=List.empty[Int]))
            case ']' =>
              if(stack.size == 2) doMagnitude(n, state)
              else                step(state.copy(pos=pos+1, depth=depth-1, stack=List.empty[Int]))
            case ',' => step(state.copy(pos=pos+1))
            case d   =>
              val nIdx = numIndex+1
              step(state.copy(pos=numbers(nIdx).end, stack=nIdx +: stack, numIndex=nIdx))
          }
      }
    }

    step(initialState(scanNumbers(n)))
  }

  def doSplit(n: SnailfishNumber, ne: NumberEntry): SnailfishNumber = {
    val left = n.splitAt(ne.start)._1
    val right = n.splitAt(ne.end)._2

    val n1 = Math.floor(ne.n / 2.0).toInt
    val n2 = Math.ceil(ne.n / 2.0).toInt

    left ++ s"[$n1,$n2]" ++ right
  }

  def trySplit(n: SnailfishNumber): SnailfishNumber = {
    val numbers = scanNumbers(n)
    numbers.find(_.n > 9) match {
      case Some(ne) => doSplit(n, ne)
      case None     => n
    }
  }

  case class NumberEntry(n: Int, start: Int, end: Int)
  val numRegex = """(\d+)""".r
  def scanNumbers(n: SnailfishNumber): Vector[NumberEntry] =
    numRegex
      .findAllMatchIn(n)
      .map(m => NumberEntry(m.toString.toInt, m.start, m.end))
      .toVector

  val numPairRegex = """(\d+,\d+)].*""".r
  def parseNumberPair(s: String): Option[String] = {
    s match {
      case numPairRegex(p) => Some(p)
      case _               => None
    }
  }

  val explodeTest1 = "[[[[[9,8],1],2],3],4]"
  val explodeTest1Result = "[[[[0,9],2],3],4]"

  val explodeTest2 = "[7,[6,[5,[4,[3,2]]]]]"
  val explodeTest2Result = "[7,[6,[5,[7,0]]]]"

  val explodeTest3 = "[[6,[5,[4,[3,2]]]],1]"
  val explodeTest3Result = "[[6,[5,[7,0]]],3]"

  val explodeTest4 = "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]"
  val explodeTest4Result = "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"

  val explodeTest5 = "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"
  val explodeTest5Result = "[[3,[2,[8,0]]],[9,[5,[7,0]]]]"

  def readData(f: String): List[SnailfishNumber] =
    io.Source.fromFile(f).getLines().toList

  def parseData(lines: Iterator[String]): List[SnailfishNumber] =
    lines.toList

  val testList1 = """[1,1]
                    |[2,2]
                    |[3,3]
                    |[4,4]""".stripMargin.linesIterator
  val testList1Result = "[[[[1,1],[2,2]],[3,3]],[4,4]]"

  val testList2 = """[1,1]
                    |[2,2]
                    |[3,3]
                    |[4,4]
                    |[5,5]""".stripMargin.linesIterator
  val testList2Result = "[[[[3,0],[5,3]],[4,4]],[5,5]]"

  val testList3 = """[1,1]
                    |[2,2]
                    |[3,3]
                    |[4,4]
                    |[5,5]
                    |[6,6]""".stripMargin.linesIterator
  val testList3Result = "[[[[5,0],[7,4]],[5,5]],[6,6]]"

  val testList4 = """[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
                    |[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
                    |[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
                    |[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
                    |[7,[5,[[3,8],[1,4]]]]
                    |[[2,[2,2]],[8,[8,1]]]
                    |[2,9]
                    |[1,[[[9,3],9],[[9,0],[0,7]]]]
                    |[[[5,[7,4]],7],1]
                    |[[[[4,2],2],6],[8,7]]""".stripMargin.linesIterator
  val testList4Result = "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"

  val testList5 = """[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
                    |[[[5,[2,8]],4],[5,[[9,9],0]]]
                    |[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
                    |[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
                    |[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
                    |[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
                    |[[[[5,4],[7,7]],8],[[8,3],8]]
                    |[[9,3],[[9,9],[6,[4,9]]]]
                    |[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
                    |[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]""".stripMargin.linesIterator
  val testList5Result = "[[[[6,6],[7,6]],[[7,7],[7,0]]],[[[7,7],[7,7]],[[7,8],[9,9]]]]"
  val testList5Magnitude = 4140

  val dataFile = "data/Day18.txt"
}
