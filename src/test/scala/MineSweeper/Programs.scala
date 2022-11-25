package MineSweeper
import scala.annotation.tailrec

object Programs {

  type Coords = (Int, Int)
  type Operation = (MineSweeperAPI, Coords) => Either[Error, MineSweeperAPI]
  type Program = List[Step]
  sealed trait Step
  case class Pick(coords: Coords) extends Step
  case class Tag(coords: Coords) extends Step



  implicit class ProgramUtils(program: List[Step]) {
    def run(initialState: MineSweeperAPI): Either[Error, MineSweeperAPI] = {
      println("Grid is: " + initialState.showResult)

      @tailrec
      // While we still have steps to run, if step is successful run next step, else return the error
      def runStep(state: MineSweeperAPI, steps: List[Step]): Either[Error, MineSweeperAPI] = {
        println(state.mkString)
        if (steps.nonEmpty) {
          (steps.head match {
            case Pick(coords) => state.pick(coords)
            case Tag(coords) => state.tag(coords)
          }) match {
            case Right(m) => runStep(m, steps.tail)
            case Left(e) => Left(e)
          }
        } else Right(state)
      }

      runStep(initialState, program)
    }
  }
  implicit class TestUtils(s: Either[Error, MineSweeperAPI]) {
    def state: MineSweeperAPI = {
      s match {
        case Right(m: MineSweeperAPI) => m
      }
    }

    def error: Error = {
      s match {
        case Left(e: Error) => e
      }
    }
  }

  val winnerProgram: Program = List(
    Pick((0, 0)),
    Pick((3, 3)),
    Pick((3, 4)),
    Pick((4, 4)),
    Pick((4, 3)),
    Pick((4, 0)),
    Tag((2, 3)),
    Tag((2, 4)),
    Tag((4, 5)),
    Tag((4, 1))
  )

  val tagAllBombs: List[Step] = List(
    Tag((2, 3)),
    Tag((2, 4)),
    Tag((4, 5)),
    Tag((4, 1))
  )

  val singleTag: List[Step] = List(
    Tag((0, 0))
  )

  val singlePick: List[Step] = List(
    Pick((0, 0))
  )

  val loserProgram: List[Step] = List(
    Pick((2, 3))
  )

  val errorOutOfBoundsProgramPick: List[Step] = List(
    Pick((99, 99))
  )

  val errorOutOfBoundsProgramTag: List[Step] = List(
    Tag((99, 99))
  )

  val errorAlreadyExposedCellPick: List[Step] = List(
    Pick((0, 0)),
    Pick((0, 0))
  )

  val errorAlreadyExposedCellTag: List[Step] = List(
    Pick((0, 0)),
    Tag((0, 0))
  )

}
