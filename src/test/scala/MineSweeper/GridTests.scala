package MineSweeper

import org.scalatest._
import flatspec._
import matchers._

class GridTests extends AnyFlatSpec with should.Matchers {
  implicit class GridUtils(grid: Grid) {
    def countBombs: Int = grid.flatten.count(cell => cell.kind == Bomb)

  }

  val easyDifficultyGame: MineSweeperAPI = MineSweeperAPI((5, 8), 1, Some(1))
  val normalDifficultyGame: MineSweeperAPI = MineSweeperAPI((5, 8), 2, Some(1))
  val hardDifficultyGame: MineSweeperAPI = MineSweeperAPI((5, 8), 3, Some(1))

  "A game" should "have the correct difficulty" in {
    easyDifficultyGame.grid.countBombs should be(4)
    normalDifficultyGame.grid.countBombs should be(6)
    hardDifficultyGame.grid.countBombs should be(8)
  }


  "A grid" should "be filled with bombs" in {
    val grid = (3, 3).initialGrid(9, None)
    val bombGrid = Array.ofDim[Cell](3, 3)
      .map(row => row.map(_ => Cell(Bomb)))
    grid should be(bombGrid)
  }

}
