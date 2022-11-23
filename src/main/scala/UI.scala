import MineSweeper._
import cats.effect.IO
import cats.implicits.catsSyntaxMonadIdOps
import scala.util.Try
import cats.syntax.all._

object UI {
  def putStrLn(value: String): IO[Unit] = IO(println(value))

  implicit class ErrorStr(msg: String) {
    def pretty: String = {
      Colors.RED_BOLD_BRIGHT + msg + Colors.RESET
    }
  }

  implicit class ErrorUtils(error: Error) {
    def mkStr: String = error match {
      case AlreadyExposedCell => "Already exposed cell".pretty
      case ArrayIndexOutOfBounds => "Coordinate out of bounds".pretty
      case NotAnOption => "Not a valid option".pretty
      case GenericError(e) => e.toString.pretty
    }
  }

  def buildMachine(): IO[MineSweeperAPI] = {
    for {
      h <- readSingleInt("Choose a grid size.\nEnter height:")
      w <- readSingleInt("Enter width:")
      d <- readSingleInt("Choose difficulty:\n1: Easy\n2: Medium\n3: Hard")
      hasSeed <- readSingleInt("Do you have a seed to build the grid?\n1: Yes\n2: No")
      machine <- if (d == 1 | d == 2 | d == 3) {
                     if (hasSeed == 1) readSingleInt("Please enter your seed:") >>= (s => IO(MineSweeperAPI((h, w), d, Some(s))))
                     else IO(MineSweeperAPI((h, w), d, None))
                 } else buildMachine()
    } yield machine
  }

  def run(machine: MineSweeperAPI): IO[Unit] = {
    machine.iterateUntilM(next(_) >>= processResponse)(_.isFinished)
      .flatMap { machine => putStrLn("Game over. Grid was: \n" + machine.showResult) }
  }

  def readSingleInt(msg: String = ""): IO[Int] = {
    IO {
      println(msg)
      Try(scala.io.StdIn.readInt).toOption
    } >>= {
      case Some(n) => IO(n)
      case _ => readSingleInt(msg)
    }
  }

  def getCoords: IO[(Int, Int)] = {
    for {
      xCoord <- readSingleInt("X coordinate:")
      yCoord <- readSingleInt("Y coordinate:")
    } yield (yCoord, xCoord)
  }

  def processResponse(res: (Either[Error, MineSweeperAPI], MineSweeperAPI)): IO[MineSweeperAPI] = {
    val (state, last) = res
    state.fold(
      (e: Error) => { putStrLn(e.mkStr) >>= (_ => next(last) >>= processResponse) },
      (m: MineSweeperAPI) => IO(m)
    )
  }

  def next(machine: MineSweeperAPI): IO[( Either[Error, MineSweeperAPI], MineSweeperAPI )] = {
    IO {
      println(machine.mkString + "\nWhat do you want to do?\n1. Discover a cell\n2. Flag/Unflag a cell")
      Try(scala.io.StdIn.readInt).toOption
    } >>= {
      case Some(1) => getCoords >>= { case (x, y) => IO((machine.pick(x, y), machine)) }
      case Some(2) => getCoords >>= { case (x, y) => IO((machine.tag(x, y), machine)) }
      case _ => next(machine)
    }
  }

}
