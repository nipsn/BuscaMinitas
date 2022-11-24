package MineSweeper

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import FinalStateCollection._

class GameTests  extends AnyFlatSpec with should.Matchers {

  val initialGrid: MineSweeperAPI = MineSweeperAPI(grid = (5, 8).initialGrid(nBombs = 4, seed = Some(1)))

  "A game" should "be won" in {
    winnerGameRoutine(initialGrid).isFinished should be(true)
  }

  "A game" should "be lost" in {
    loserGameRoutine(initialGrid).isFinished should be(true)
  }

  "A game" should "be unfinished after pick on non-Bomb cell" in {
    partialGameRoutinePick(initialGrid).isFinished should be(false)
  }

  "A game" should "be unfinished after tag" in {
    partialGameRoutineTag(initialGrid).isFinished should be(false)
  }

}
