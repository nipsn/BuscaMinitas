package MineSweeper

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import Programs._


class GameTests  extends AnyFlatSpec with should.Matchers {

    val initial: MineSweeperAPI = MineSweeperAPI(grid = (5, 8).initialGrid(nBombs = 4, seed = Some(1)))

    "A game" should "be won" in {
      winnerProgram.run(initial).state.isFinished should be(true)
    }

    "A game" should "be lost" in {
      loserProgram.run(initial).state.isFinished should be(true)
    }

    "A game" should "be unfinished" in {
      singlePick.run(initial).state.isFinished should be(false)
      singleTag.run(initial).state.isFinished should be(false)
      tagAllBombs.run(initial).state.isFinished should be(false)
    }

    "A move" should "result in error" in {
      // Out of bounds pick or tag
      errorOutOfBoundsProgramPick.run(initial).error should be(ArrayIndexOutOfBounds)
      errorOutOfBoundsProgramTag.run(initial).error should be(ArrayIndexOutOfBounds)

      // Already exposed cell. Picking or tagging a visible cell is pointless
      errorAlreadyExposedCellPick.run(initial).error should be(AlreadyExposedCell)
      errorAlreadyExposedCellTag.run(initial).error should be(AlreadyExposedCell)
    }
}
