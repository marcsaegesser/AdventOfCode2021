package advent

import scala.math.BigInt

object Day17 {

  def run(currX: Int, currY: Int, xV: Int, yV: Int, target: Target): Boolean = {
    val x = currX + xV
    val y = currY + yV
    if(x <= target.xTarget.max && y >= target.yTarget.min)
      run(x, y, Math.max(0, xV-1), yV - 1, target)
    else
      target.xTarget.contains(currX) && target.yTarget.contains(currY)
  }

  def part2(target: Target) = {

    val maxVY = maxYVelo(target.yTarget)._1
    val xVelos = allowedXVelos(target.xTarget).map(_._1)
    val yVelos = (-10 to maxVY)

    yVelos.map(vy => xVelos.filter(vx => run(0, 0, vx, vy, target)).size).sum
  }

  def findValid(target: Target) = {
    val maxVY = maxYVelo(target.yTarget)._1
    val xVelos = allowedXVelos(target.xTarget).map(_._1)
    val yVelos = (target.yTarget.min to maxVY)
    yVelos
      .map(vy => xVelos.filter(vx => checkVelo(vx, vy, target)).size)
      .sum
    // yVelos
    //   .flatMap(vy => xVelos.map(_._1).map(vx => (vx, vy))
    //     .filter(vs => checkVelo(vs._1, vs._2, xTarget, yTarget)))
  }

  def checkVelo(vX: Int, vY: Int, target: Target): Boolean = {
    lazyCoords(vX, vY, target)
      .toList
      // .takeWhile((x, y) => x <= xTarget.max && y >= yTarget.min)
      .map((x, y) => target.xTarget.contains(x) && target.yTarget.contains(y))
      .last
  }

  def maxYVelo(target: Range) = {
    LazyList.from(1)
      .map(v => (v, maxYForVelo(v)))
      .map((v, y) => (v, y, (LazyList.from(0).map(n => LazyList.from(1).take(n).sum).map(y - _).takeWhile(_ >= target.min).toList)))
      .takeWhile((v, y, ys) => ys.last < 0)
      .filterNot((v, y, ys) => target.intersect(ys).isEmpty)
      .toList
      .last
  }

  def maxYForVelo(v: Int): Int =
    (1 to v).sum

  def allowedXVelos(target: Range) = {
    (1 to target.max)
      .map(v => (v, xsForVelo(v).dropWhile(_ > target.max)))
      .filterNot((v, xs) => target.intersect(xs).isEmpty)
      // .head
      // ._1
  }


  def lazyXs(v: Int): LazyList[Int] =
    xsForVelo(v) match {
      case Nil                          => LazyList.empty[Int]
      case x1 :: Nil                    => LazyList.empty[Int]
      case x1 :: x2 :: t if(x1-x2 == 1) => LazyList((x2 :: t).reverse:_*) ++: LazyList.continually(x1)
      case xs                           => LazyList(xs.reverse:_*)
    }

  def lazyYs(v: Int) = {
    def ys: LazyList[(Int, Int)] = (0, v) #:: (v, v-1) #:: ys.zip(ys.tail).map((_, n) => (n._1+n._2, n._2-1))
    ys.map(_._1)
  }

  def lazyCoords(vX: Int, vY: Int, target: Target) =
    lazyXs(vX).takeWhile(_ <= target.xTarget.max).zip(lazyYs(vY).takeWhile(_ >= target.yTarget.min))

  def xsForVelo(v: Int): List[Int] =
    LazyList.from(1)
      .take(v)
      .toList
      .reverse
      .foldLeft(List(0)) { case (l, x) => (x + l.head) +: l }

  case class Target(xTarget: Range, yTarget: Range)

  val targetRegex = """target area: x=(-?\d+)..(-?\d+), y=(-?\d+)..(-?\d+)""".r

  def parseData(data: String): Target = {
    val targetRegex(xmin, xmax, ymin, ymax) = data
    Target((xmin.toInt to xmax.toInt), (ymin.toInt to ymax.toInt))
  }

  val testData = "target area: x=20..30, y=-10..-5"
  val data = "target area: x=56..76, y=-162..-134"
}
