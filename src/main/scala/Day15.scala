package advent

import math.Integral.Implicits.infixIntegralOps
import collection.mutable.PriorityQueue
import collection.mutable.HashMap

object Day15 {

  def run(): Unit = {
    val map = readData(dataFile)
    println(s"Day15.part1 = ${part1(map)}")
    println(s"Day15.part2 = ${part2(map)}")
  }

  def part1(map: RiskMap): Int =
    findPath(map, Coord(0, 0), (Coord(map.rows-1, map.cols-1)))

  def part2(map: RiskMap): Int = {
    val tiled = TiledMap(map, 5)
    findPath(tiled, Coord(0, 0), (Coord(tiled.rows-1, tiled.cols-1)))
  }

  given AOrdering: Ordering[(Coord, Int)] =  Ordering.by[(Coord, Int), Int](_._2)(Ordering[Int].reverse)

  def dist(c1: Coord, c2: Coord): Int =
    Math.abs(c2.r - c1.r) + Math.abs(c2.c - c1.c)

  def findPath(map: RiskMap, start: Coord, goal: Coord): Int = {
    def AStar(open: PriorityQueue[(Coord, Int)], gScore: HashMap[Coord, Int], fScore: HashMap[Coord, Int], h: (Coord, Coord) => Int): Int = {
      val (current, f) = open.dequeue
      if(current == goal) gScore(current)
      else {
        map.adjacent(current).foreach { c =>
          val t = gScore(current) + map.at(c)
          if(t < gScore.getOrElse(c, Integer.MAX_VALUE)) {
            gScore += ((c, t))
            fScore += ((c, t + h(c, goal)))
            if(!open.exists(_._1 == c))
              open += ((c, fScore(c)))
          }
        }
        AStar(open, gScore, fScore, h)
      }
    }

    AStar(PriorityQueue((start, 0)), HashMap(start -> 0), HashMap(start -> dist(start, goal)), dist)
  }

  sealed trait RiskMap {
    def rows: Int
    def cols: Int
    def at(pos: Coord): Int
    def adjacent(coord: Coord): Set[Coord]
  }

  case class BasicMap(map: Vector[Vector[Int]], rows: Int, cols: Int) extends RiskMap {
    def at(pos: Coord): Int =
      map(pos.r)(pos.c)

    def adjacent(coord: Coord): Set[Coord] =
      coord.adjacencies.filter { case Coord(r, c) => map.isDefinedAt(r) && map(r).isDefinedAt(c) }
  }

  case class TiledMap(map: RiskMap, tiles: Int) extends RiskMap {

    def rows: Int = map.rows * tiles
    def cols: Int = map.cols * tiles

    def at(pos: Coord): Int = {
      val (rq, rr) = pos.r /% map.rows
      val (cq, cr) = pos.c /% map.cols
      val (a, b) = (map.at(Coord(rr, cr)) + rq + cq) /% 10
      a + b
    }

    def adjacent(coord: Coord): Set[Coord] =
      coord.adjacencies.filter { case Coord(r, c) => r >= 0 && r < rows && c >= 0 && c < cols}
  }

  case class Coord(r: Int, c: Int) {
    def adjacencies: Set[Coord] =
      Set(Coord(r-1, c), Coord(r, c+1), Coord(r+1, c), Coord(r, c-1))
  }

  def readData(f: String): RiskMap =
    parseData(io.Source.fromFile(f).getLines)

  def parseData(lines: Iterator[String]): RiskMap = {
    val map = lines.map(_.toVector.map(_.asDigit)).toVector
    BasicMap(map, map.size, map(0).size)
  }

  def testData = """1163751742
                   |1381373672
                   |2136511328
                   |3694931569
                   |7463417111
                   |1319128137
                   |1359912421
                   |3125421639
                   |1293138521
                   |2311944581""".stripMargin.linesIterator

  val dataFile = "data/Day15.txt"
}
