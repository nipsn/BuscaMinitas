import MineSweeper._
import cats.effect.{ExitCode, IO, IOApp}

object Main extends IOApp {
  def run(args: List[String]): IO[ExitCode] = {
    for {
      machine <- UI.buildMachine()
      _ <- play(machine)
    } yield ExitCode.Success
  }

  def play(config: MineSweeperAPI): IO[Unit] = UI.run(config)

}