package advent

object Day02 {

  def run(): Unit = {
    val commands = readData(dataFile)
    println(s"Day02.part1 = ${part1(commands)}")
    println(s"Day02.part2 = ${part2(commands)}")
  }

  def part1(commands: List[Command]): Int = {
    val state =performCommands(SubmarineState(), commands)
    state.x * state.depth
  }

  def part2(commands: List[Command]): Int = {
    val state =performCommands2(SubmarineState(), commands)
    state.x * state.depth
  }


  enum Command {
    case Forward(n: Int)
    case Down(n: Int)
    case Up(n: Int)
  }

  case class SubmarineState(x: Int = 0, depth: Int = 0, aim: Int = 0)

  def performCommands(state: SubmarineState, commands: List[Command]): SubmarineState =
    commands.foldLeft(state) { performCommand }

  def performCommand(state: SubmarineState, command: Command): SubmarineState = {
    import Command._
    command match {
      case Forward(n) => state.copy(x=state.x+n)
      case Down(n)    => state.copy(depth=state.depth+n)
      case Up(n)      => state.copy(depth=state.depth-n)
    }
  }

  def performCommands2(state: SubmarineState, commands: List[Command]): SubmarineState =
    commands.foldLeft(state) { performCommand2 }

  def performCommand2(state: SubmarineState, command: Command): SubmarineState = {
    import Command._
    command match {
      case Forward(n) => state.copy(x=state.x+n, depth=state.depth+(n*state.aim))
      case Down(n)    => state.copy(aim=state.aim+n)
      case Up(n)      => state.copy(aim=state.aim-n)
    }
  }

  val commandRegex = """(\w+)\s+(\d+)""".r

  def parseCommand(s: String): Command =
    s match {
      case commandRegex("forward", n) => Command.Forward(n.toInt)
      case commandRegex("down", n)    => Command.Down(n.toInt)
      case commandRegex("up", n)      => Command.Up(n.toInt)
    }

  def readData(f: String): List[Command] =
    parseData(io.Source.fromFile(f).getLines.toList)

  def parseData(data: List[String]): List[Command] =
    data.map(parseCommand)

  def testData =
    """forward 5
      |down 5
      |forward 8
      |up 3
      |down 8
      |forward 2""".stripMargin.linesIterator.toList

  val dataFile = "data/Day02.txt"
}
