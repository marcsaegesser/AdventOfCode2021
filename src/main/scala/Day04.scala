package advent

import advent.Day04.BingoBoard

object Day04 {

  def run(): Unit = {
    val game = readData(dataFile)
    println(s"Day04.part1 = ${findWinningBoard(game)}")
    println(s"Day04.part2 = ${findLastWinningBoard(game)}")
  }

  def findWinningBoard(game: BingoGame): Option[Int] = {
    def helper(g: BingoGame): Option[(Int)] = {
      playRound(g).flatMap(n =>
        n.boards.find(isWinner)
          .map(w => score(g.numbers.head, w))
          .orElse(helper(n))
      )
    }

    helper(game)
  }

  def findLastWinningBoard(game: BingoGame): Option[Int] = {
    def helper(g: BingoGame, prev: Option[Int] ): Option[Int] = {
      if(g.boards.isEmpty) prev
      else
        playRound(g).flatMap(next =>
          val (w, l) = next.boards.partition(isWinner)
            w.lastOption match {
              case None    => helper(next, prev)
              case Some(b) => helper(next.copy(boards=l), Some(score(g.numbers.head, b)))
            }
        )
    }

    helper(game, None)
  }

  def score(n: Int, board: BingoBoard): Int = {
    board.board.filterNot((n, c) => board.hits.contains(c)).map(_._1).sum * n
  }

  def playRound(game: BingoGame): Option[BingoGame] = {
    game.numbers match {
      case Nil     => None
      case n :: ns => Some(BingoGame(ns, game.boards.map(b => playNumber(n, b))))
    }
  }

  def playNumber(n: Int, board: BingoBoard): BingoBoard = {
    board.board.get(n)
      .map { case Coord(r, c) => board.copy(hits=Coord(r, c) +: board.hits, rowCounts=incrCount(r, board.rowCounts),columnCounts=incrCount(c, board.columnCounts))}
    .getOrElse(board)
  }

  def isWinner(board: BingoBoard): Boolean =
    board.rowCounts.exists(_ == 5) || board.columnCounts.exists(_ == 5)

  def incrCount(n: Int, cs: Vector[Int]): Vector[Int] =
    cs.updated(n, cs(n)+1)

  case class Coord(r: Int, c: Int)

  case class BingoGame(numbers: List[Int], boards: List[BingoBoard])

  case class BingoBoard(board: Map[Int, Coord], hits: List[Coord], rowCounts: Vector[Int], columnCounts: Vector[Int])

  def parseNumbers(line: String): List[Int] =
    line.split(",").map(_.toInt).toList

  val boardRegex = """\s*(\d+)""".r

  def parseBoard(lines: Seq[String]): BingoBoard = {
    val board =
      lines.takeWhile(!_.isEmpty)
        .flatMap(boardRegex.findAllMatchIn)
        .map(_.group(1))
        .map(_.toInt)
        .grouped(5)
        .zipWithIndex
        .flatMap((ns, r) => ns.zipWithIndex.map((n, c) => (n, Coord(r, c))))
        .toMap

    BingoBoard(board, List.empty[Coord], Vector.fill(5)(0), Vector.fill(5)(0))
  }

  def parseData(lines: Iterator[String]): BingoGame = {
    val (ns, bs) = lines.splitAt(1)
    val numbers = parseNumbers(ns.next)
    val boards = bs.filterNot(_.isEmpty).grouped(5).map(parseBoard)
    BingoGame(numbers, boards.toList)
  }

  def readData(f: String): BingoGame =
    parseData(io.Source.fromFile(f).getLines)

  def testData = """7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1
                   |
                   |22 13 17 11  0
                   | 8  2 23  4 24
                   |21  9 14 16  7
                   | 6 10  3 18  5
                   | 1 12 20 15 19
                   |
                   | 3 15  0  2 22
                   | 9 18 13 17  5
                   |19  8  7 25 23
                   |20 11 10 24  4
                   |14 21 16 12  6
                   |
                   |14 21 17 24  4
                   |10 16 15  9 19
                   |18  8 23 26 20
                   |22 11 13  6  5
                   | 2  0 12  3  7""".stripMargin.linesIterator

  val dataFile = "data/Day04.txt"
}
