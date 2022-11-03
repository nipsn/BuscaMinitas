import MineSweeper.{AlreadyExposedCell, AlreadyTaggedCell, ArrayIndexOutOfBounds, GenericError, MineSweeperAPI, NotAnOption}
import cats.effect.IO
import cats.implicits.catsSyntaxMonadIdOps
import MineSweeper.Error
import MineSweeper.Colors

object UI {
  val readLn: IO[String] = IO(scala.io.StdIn.readLine())

  def putStrLn(value: String): IO[Unit] = IO(println(value))

  implicit class ErrorStr(msg: String) {
    def pretty: String = {
      Colors.RED_BOLD_BRIGHT + msg + Colors.RESET
    }
  }

  implicit class ErrorUtils(error: Error) {
    def mkStr: String = error match {
      case AlreadyExposedCell => "Already exposed cell".pretty
      case AlreadyTaggedCell => "Already tagged cell".pretty
      case ArrayIndexOutOfBounds => "Coordinate out of bounds".pretty
      case NotAnOption => "Not a valid option".pretty
      case GenericError(e) => e.toString.pretty
    }
  }

  def buildMachine(): IO[MineSweeperAPI] = {
    putStrLn("Choose a grid size.\nEnter height:") flatMap
      (_ => readLn) flatMap
      (h => putStrLn("Enter width:") flatMap
        (_ => readLn) flatMap
        (w => putStrLn("Choose difficulty:\n1: Easy\n2: Medium\n3: Hard")
          flatMap (_ => readLn) flatMap
          (d => {
            if (d == "1" | d == "2" | d == "3") {
              IO(MineSweeperAPI((h.toInt, w.toInt), d.toInt))
            } else buildMachine()
          })))
  }


  def run(machine: MineSweeperAPI): IO[Unit] = {
    machine.iterateUntilM(machine => console(machine))(machine => machine.isFinished)
      .flatMap { machine => putStrLn("Game over. Grid was: \n" + machine.showResult) }
  }


  def console(machine: MineSweeperAPI): IO[MineSweeperAPI] = {
    putStrLn(machine.mkString + "\nWhat do you want to do?\n1. Discover a cell\n2. Flag/Unflag a cell") flatMap
      (_ => readLn) flatMap
      (op => putStrLn("X coordinate:") flatMap
        (_ => readLn) flatMap
        (x => putStrLn("Y coordinate:")
          flatMap (_ => readLn) flatMap
          (y => {
            if (op == "1" | op == "2") {
              val state = op match {
                case "1" => machine.pick(x.toInt, y.toInt)
                case "2" => machine.tag(x.toInt, y.toInt)
              }
              state.fold(
                (e: Error) => {
                  // TODO: fix out of bounds error
                  putStrLn(e.mkStr).flatMap(_ => console(machine))
                },
                (m: MineSweeperAPI) => IO(m))
            } else console(machine)
          })))
  }


}
