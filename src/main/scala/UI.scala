import MineSweeper.{AlreadyExposedCell, AlreadyTaggedCell, ArrayIndexOutOfBounds, GenericError, MineSweeperAPI, NotAnOption}
import cats.effect.IO
import cats.implicits.catsSyntaxMonadIdOps
import MineSweeper.Error
import MineSweeper.Colors

import scala.util.Try
import cats.syntax.all._

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
    for {
      h <- readSingleInt("Choose a grid size.\nEnter height:")
      w <- readSingleInt("Enter width:")
      d <- readSingleInt("Choose difficulty:\n1: Easy\n2: Medium\n3: Hard")
      machine <- if (d == 1 | d == 2 | d == 3) IO(MineSweeperAPI((h, w), d))
                 else buildMachine()
    } yield machine
  }


  def run(machine: MineSweeperAPI): IO[Unit] = {
    machine.iterateUntilM(console(_) >>= processResponse)(_.isFinished)
      .flatMap { machine => putStrLn("Game over. Grid was: \n" + machine.showResult) }
  }

  def readSingleInt(msg: String): IO[Int] = {
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
    } yield (xCoord, yCoord)
  }

  def processResponse(state: (Either[Error, MineSweeperAPI], MineSweeperAPI)): IO[MineSweeperAPI] = {
    state._1.fold(
      (e: Error) => { putStrLn(e.mkStr).flatMap(_ => console(state._2) >>= processResponse) },
      (m: MineSweeperAPI) => IO(m)
    )
  }

  def console(machine: MineSweeperAPI): IO[(Either[Error, MineSweeperAPI], MineSweeperAPI)] = {
    IO {
      println(machine.mkString + "\nWhat do you want to do?\n1. Discover a cell\n2. Flag/Unflag a cell")
      Try(scala.io.StdIn.readInt).toOption
    } >>= {
      case Some(1) => getCoords >>= { case (x, y) => IO((machine.pick(x, y), machine)) }
      case Some(2) => getCoords >>= { case (x, y) => IO((machine.tag(x, y), machine)) }
      case _ => console(machine)
    }
  }

}
