import MineSweeper.{AlreadyExposedCell, AlreadyTaggedCell, ArrayIndexOutOfBounds, GenericError, MineSweeperAPI, NotAnOption}
import cats.effect.IO
import cats.implicits.catsSyntaxMonadIdOps
import cats.effect.unsafe.implicits.global
import MineSweeper.Error
import MineSweeper.Colors

object UI {
  val readLn: IO[String] = IO(scala.io.StdIn.readLine())

  def putStrLn(value: String): IO[Unit] = IO(println(value))

  implicit class errorMsg(msg: String) {
    def pretty: String = {
      Colors.RED_BOLD_BRIGHT + msg + Colors.RESET
    }
  }

  def runInit(): IO[MineSweeperAPI] = {
    for {
      _ <- putStrLn("Choose a grid size.\nEnter height:")
      height <- readLn
      _ <- putStrLn("Enter width:")
      length <- readLn
      _ <- putStrLn("Choose difficulty:\n1: Easy\n2: Medium\n3: Hard")
      difficulty <- readLn
    } yield {
      val totalCells = height.toInt * length.toInt
      if (difficulty == "1") {
        MineSweeperAPI((height.toInt, length.toInt), (totalCells * 0.1).toInt)
      } else if (difficulty == "2") {
        MineSweeperAPI((height.toInt, length.toInt), (totalCells * 0.15).toInt)
      } else if (difficulty == "3") {
        MineSweeperAPI((height.toInt, length.toInt), (totalCells * 0.2).toInt)
      } else {
        putStrLn("Not a valid option. Retry.")
        runInit().unsafeRunSync()
      }
    }
  }


  def run(machine: MineSweeperAPI): IO[Unit] = {
    machine.iterateUntilM(machine => console(machine))(machine => machine.isFinished)
      .flatMap { machine => putStrLn("Game over. Grid was: \n" + machine.mkString) }
  }


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
    } fold(
      (e: Error) => {
        // TODO: fix out of bounds error
        putStrLn(e match {
          case AlreadyExposedCell => "Already exposed cell".pretty
          case AlreadyTaggedCell => "Already tagged cell".pretty
          case ArrayIndexOutOfBounds => "Coordinate out of bounds".pretty
          case NotAnOption => "Not a valid option".pretty
          case GenericError(e) => e.toString.pretty
        }).flatMap(_ => console(machine)).unsafeRunSync()
      },
      (m: MineSweeperAPI) => m
    )
  }


}
