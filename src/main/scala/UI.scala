import MineSweeper.{AlreadyExposedCell, AlreadyTaggedCell, ArrayIndexOutOfBounds, GenericError, MineSweeperAPI, NotAnOption}
import cats.effect.IO
import cats.implicits.catsSyntaxMonadIdOps

import scala.io.StdIn.readLine

object UI {
  val readLn: IO[String] = IO(scala.io.StdIn.readLine())

  def run(machine: MineSweeperAPI): IO[Unit] = {
    machine.iterateUntilM(machine => console(machine))(machine => machine.isFinished)
      .flatMap { machine => putStrLn("Game over. Grid was: \n" + machine.mkString) }
  }

  def putStrLn(value: String): IO[Unit] = IO(println(value))

  def console(machine: MineSweeperAPI): IO[MineSweeperAPI] = {
    for {
      _ <- putStrLn(machine.mkString + "\nWhat do you want to do?\n1. Discover a cell\n2. Flag/Unflag a cell")
      operation <- readLn
      _ <- putStrLn("X coordinate:")
      xcoord <- readLn
      _ <- putStrLn("Y coordinate:")
      ycoord <- readLn
    } yield {
      if (operation == "1") {
        machine.pick(xcoord.toInt, ycoord.toInt)
      } else if (operation == "2") {
        machine.tag(xcoord.toInt, ycoord.toInt)
      } else Left(NotAnOption)
    } match {
      // TODO: proper error printing
      case Left(e) =>
        val out = e match {
          case AlreadyExposedCell => "Already exposed cell"
          case AlreadyTaggedCell => "Already tagged cell"
          case ArrayIndexOutOfBounds => "Coordinate out of bounds"
          case NotAnOption => "Not a valid option"
          case GenericError(e) => e.toString
        }
        putStrLn(out)
        machine
      case Right(m) => m
    }
  }


}
