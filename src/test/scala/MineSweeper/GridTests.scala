package MineSweeper

import org.scalatest._
import flatspec._
import matchers._

class GridTests extends AnyFlatSpec with should.Matchers {
  "A grid" should "be filled with bombs" in {
    val grid = (3, 3).initialGrid(9, None)
    val bombGrid = Array.ofDim[Cell](3, 3)
      .map(row => row.map(_ => Cell(Bomb)))
    grid should be(bombGrid)
  }

}
