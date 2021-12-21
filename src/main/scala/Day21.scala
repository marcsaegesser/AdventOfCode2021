package advent

import scala.languageFeature.postfixOps

object Day21 {

  def run(): Unit = {
    val (p1, p2) = parseData(data)
    println(s"Day21, part1 = ${part1(p1, p2)}")
    println(s"Day21, part2 = ${part2(p1, p2)}")
  }

  def part1(p1: Int, p2: Int): Int = {
    val result = playGame1(p1, p2)
    Math.min(result.score1, result.score2) * result.rolls
  }

  def part2(p1: Int, p2: Int): Long = {
    val (c1, c2) = playGame2(p1, p2)
    Math.max(c1, c2)
  }

  val occurrences = Vector(0, 0, 0, 1, 3, 6, 7, 6, 3, 1)

  val rolls = List(3, 4, 5, 6, 7, 8, 9)


  case class State(pos1: Int, score1: Int, pos2: Int, score2: Int, next: Int, universeCount: Long)

  def run(accum: (Long, Long), state: State): (Long, Long) = {
    if(state.score1 >= 21)      (accum._1+state.universeCount, accum._2)
    else if(state.score2 >= 21) (accum._1, accum._2+state.universeCount)
    else                        step(state).foldLeft(accum) { case (a, s) => run(a, s) }
  }

  def playGame2(p1: Int, p2: Int): (Long, Long) = {
    run((0L, 0L), State(p1-1, 0, p2-1, 0, 0, 1))
  }

  def step(state: State): List[State] = {
    state match { case State(p1, s1, p2, s2, n, c) =>
      rolls.map { r =>
        if(n == 0) {
          val nextPos = (p1 + r) % 10
          val nextScore = s1 + nextPos + 1
          state.copy(pos1=nextPos, score1=nextScore, next=(n+1)%2, universeCount=c*(occurrences(r)))
        } else {
          val nextPos = (p2 + r) % 10
          val nextScore = s2 + nextPos + 1
          state.copy(pos2=nextPos, score2=nextScore, next=(n+1)%2, universeCount=c*(occurrences(r)))
        }
      }
    }
  }


  // A reall stupid LazyList solution to part1

  case class State1(pos1: LazyList[Int], score1: Int, pos2: LazyList[Int], score2: Int, die: LazyList[Int], rolls: Int, next: Int)

  def playGame1(p1: Int, p2: Int): State1 = {
    def turn(state: State1): State1 = {
      state match { case State1(p1, s1, p2, s2, die, rolls, next) =>
        if(s1 >= 1000 || s2 >= 1000) state
        else {
          val roll = die.take(3).toList.sum
          if(next == 0) {
            val nextPos = p1.drop(roll)
            val nextScore = s1 + nextPos.head
            turn(state.copy(pos1=nextPos, score1=nextScore, die=die.drop(3), rolls=rolls+3, (next+1)%2))
          } else {
            val nextPos = p2.drop(roll)
            val nextScore = s2 + nextPos.head
            turn(state.copy(pos2=nextPos, score2=nextScore, die=die.drop(3), rolls=rolls+3, (next+1)%2))
          }
        }
      }
    }

    turn(State1(
      LazyList.continually(1 to 10).flatten.drop(p1-1),
      0,
      LazyList.continually(1 to 10).flatten.drop(p2-1),
      0,
      LazyList.continually(1 to 100).flatten,
      0,
      0))
  }

  val playerRegex = """.*:\s+(\d)""".r

  def parseData(lines: Iterator[String]): (Int, Int) = {
    val ls = lines.toVector
    val playerRegex(p1) = ls(0)
    val playerRegex(p2) = ls(1)
    (p1.toInt, p2.toInt)
  }

  def testData = """Player 1 starting position: 4
                   |Player 2 starting position: 8""".stripMargin.linesIterator

  def data = """Player 1 starting position: 8
               |Player 2 starting position: 5""".stripMargin.linesIterator
}
