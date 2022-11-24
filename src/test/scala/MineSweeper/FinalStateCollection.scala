package MineSweeper
import Programs._

object FinalStateCollection {
  def winnerGameState(m: MineSweeperAPI): MineSweeperAPI = {
    winnerGameRoutineSteps.runS(m).value
  }

  def loserGameState(m: MineSweeperAPI): MineSweeperAPI = {
    loserGameRoutineSteps.runS(m).value
  }

  def partialGameStatePick(m: MineSweeperAPI): MineSweeperAPI = {
    partialGameRoutinePickSteps.runS(m).value
  }

  def partialGameStateTag(m: MineSweeperAPI): MineSweeperAPI = {
    partialGameRoutineTagSteps.runS(m).value
  }
}
