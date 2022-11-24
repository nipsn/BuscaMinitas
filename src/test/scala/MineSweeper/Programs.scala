package MineSweeper
import cats.data.State
import cats.data.State._

object Programs {

  implicit class TestUtils(state: Either[Error, MineSweeperAPI]) {
    // ignora los errores y devuelve la maquina
    // se que con mis operaciones en este fichero no van a saltar errores a menos que sean intencionados
    def getMineSweeperAPI: MineSweeperAPI = {
      state match {
        case Right(m: MineSweeperAPI) => m
      }
    }
  }

  def partialGameRoutineTagSteps: State[MineSweeperAPI, Unit] = {
    for {
      _ <- modify[MineSweeperAPI](_.tag(0, 0).getMineSweeperAPI)
    } yield get[MineSweeperAPI]
  }

  def partialGameRoutinePickSteps: State[MineSweeperAPI, Unit] = {
    for {
      _ <- modify[MineSweeperAPI](_.pick(0, 0).getMineSweeperAPI)
    } yield get[MineSweeperAPI]
  }

  def loserGameRoutineSteps: State[MineSweeperAPI, Unit] = {
    for {
      _ <- modify[MineSweeperAPI](_.pick(2, 3).getMineSweeperAPI)
    } yield get[MineSweeperAPI]
  }

  def winnerGameRoutineSteps: State[MineSweeperAPI, Unit] = {
    for {
      _ <- modify[MineSweeperAPI](_.pick(0, 0).getMineSweeperAPI)
      _ <- modify[MineSweeperAPI](_.tag(2, 3).getMineSweeperAPI)
      _ <- modify[MineSweeperAPI](_.tag(2, 4).getMineSweeperAPI)
      _ <- modify[MineSweeperAPI](_.pick(3, 3).getMineSweeperAPI)
      _ <- modify[MineSweeperAPI](_.pick(3, 4).getMineSweeperAPI)
      _ <- modify[MineSweeperAPI](_.tag(4, 5).getMineSweeperAPI)
      _ <- modify[MineSweeperAPI](_.pick(4, 4).getMineSweeperAPI)
      _ <- modify[MineSweeperAPI](_.pick(4, 3).getMineSweeperAPI)
      _ <- modify[MineSweeperAPI](_.tag(4, 1).getMineSweeperAPI)
      _ <- modify[MineSweeperAPI](_.pick(4, 0).getMineSweeperAPI)
    } yield get[MineSweeperAPI]
  }
}
