package advent

object Day16 {

  def run(): Unit = {
    val data = readData(dataFile)
    println(s"Day16.part1 = ${part1(data)}")
    println(s"Day16.part2 = ${part2(data)}")
  }

  def part1(data: String): Long = {
    val p = parse(toBinary(data))._1.head
    fold(p)(0L) { (a, p) => a + p.version }
  }

  def part2(data: String): Long = {
    eval(parse(toBinary(data))._1.head)
  }

  def eval(packet: Packet): Long = {
    packet match {
      case Literal(_, n)      => n
      case Operator(_, 0, ps) => ps.map(eval).sum  // Sum
      case Operator(_, 1, ps) => ps.map(eval).reduce(_*_) // Mul
      case Operator(_, 2, ps) => ps.map(eval).min // Min
      case Operator(_, 3, ps) => ps.map(eval).max // Max
      case Operator(_, 5, ps) => ps.map(eval) match { case a :: b :: _ if a > b => 1; case _ => 0 } // Less
      case Operator(_, 6, ps) => ps.map(eval) match { case a :: b :: _ if a < b => 1; case _ => 0 } // Greater
      case Operator(_, 7, ps) => ps.map(eval) match { case a :: b :: _ if a == b => 1; case _ => 0 } // Equal
      case Operator(_, _, _)  => ??? // Invalid input
    }
  }

  def fold[A](packet: Packet)(z: A)(f: ((A, Packet) => A)): A = {
    packet match {
      case Literal(v, n) => f(z, packet)
      case Operator(v, o, ps) => f(ps.foldLeft(z) { case (a, p) => fold(p)(a)(f) }, packet)
    }
  }

  sealed trait Packet { def version: Int }

  case class Literal(version: Int, n: Long) extends Packet
  case class Operator(version: Int, operator: Int, packets: List[Packet]) extends Packet

  def parse(in: String): (List[Packet], String) = {
    def helper(accum: List[Packet], s: String): (List[Packet], String) = {
      if(!s.exists(_ != '0')) (accum.reverse, s)  // Empty or only zeros
      else {
        val (p, r) = parsePacket(s)
        helper(p +: accum, r)
      }
    }

    helper(List.empty[Packet], in)
  }

  def parseN(n: Int, in: String): (List[Packet], String) = {
    def helper(accum: List[Packet], s: String, i: Int): (List[Packet], String) =
      if(i == 0) (accum.reverse, s)
      else {
        val (p, r) = parsePacket(s)
        helper(p +: accum, r, i-1)
      }

    helper(List.empty[Packet], in, n)
  }

  def parsePacket(in: String): (Packet, String) = {
    val v = fromBinary(in.take(3)).toInt
    val t = fromBinary(in.drop(3).take(3)).toInt

    t match {
      case 4 => parseLiteral(v, in.drop(6))
      case _ => parseOperator(v, t, in.drop(6))
    }
  }

  def parseLiteral(v: Int, in: String): (Packet, String) = {
    def helper(accum: List[String], gs: List[String]): (Long, String) =
      gs match {
        case Nil => ??? // Bug
        case g :: t if g.startsWith("0") => (fromBinary(pad((g.tail +: accum).reverse.mkString)), t.mkString)
        case g :: t => helper(g.tail +: accum, t)
      }

    val (n, r) = helper(List.empty[String], in.grouped(5).toList)

    (Literal(v, n), r)
  }

  def parseOperator(v: Int, op: Int, in: String): (Packet, String) = {
    in.head.match {
      case '0' => parseOperatorByLength(v, op, in.drop(1))
      case '1' => parseOperatorByCount(v, op, in.drop(1))
      case _   => ??? // BUG
    }
  }

  def parseOperatorByLength(v: Int, op: Int, in: String): (Packet, String) = {
    val (l, d) = in.splitAt(15)
    val length = fromBinary(l).toInt
    val (ps, rs) = d.splitAt(length)

    val (p, _) = parse(ps)
    (Operator(v, op, p), rs)
  }

  def parseOperatorByCount(v: Int, op: Int, in: String): (Packet, String) = {
    val (ns, d) = in.splitAt(11)
    val n = fromBinary(ns)
    val (ps, rs) = parseN(n.toInt, d)
    (Operator(v, op, ps), rs)
  }


  def pad(s: String): String = {
    val n = 4 - (s.size % 4)
    List.fill(n)('0').mkString + s
  }

  def toBinary(in: String): String =
    in.flatMap(c => c.asDigit.toBinaryString.reverse.padTo(4, '0').reverse)

  def fromBinary(in: String): Long =
    java.lang.Long.parseLong(in, 2)

  def readData(f: String): String =
    io.Source.fromFile(f).getLines.mkString

  val testData1 = """8A004A801A8002F478"""
  val testData2 = """620080001611562C8802118E34"""
  val testData3 = """C0015000016115A2E0802F182340"""
  val testData4 = """A0016C880162017C3686B18A3D4780"""

  val dataFile = "data/Day16.txt"
}
