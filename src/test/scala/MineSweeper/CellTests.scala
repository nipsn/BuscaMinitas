package MineSweeper

import org.scalatest._
import flatspec._
import matchers._

class CellTests extends AnyFlatSpec with should.Matchers {
  val n = 1
  val bombCell: Cell = Cell(Bomb)
  val emptyCell: Cell = Cell(Empty)
  val numberedCell: Cell = Cell(Numbered(n))

  "A cell" should "be visible" in {
    bombCell.makeVisible.visible should be(true)
    emptyCell.makeVisible.visible should be(true)
    numberedCell.makeVisible.visible should be(true)

    bombCell.makeVisible.toString should be("B")
    emptyCell.makeVisible.toString should be("_")
    numberedCell.makeVisible.toString should be(n.toString)
  }

  "A cell" should "be marked" in {
    emptyCell.changeTag.tagged should be(true)
    emptyCell.changeTag.changeTag.tagged should be(false)

    emptyCell.changeTag.toString should be("λ")
    emptyCell.changeTag.changeTag.toString should be(" ")
  }

  "A cell" should "be properly constructed" in {
    bombCell should be(Cell(visible = false, tagged = false, Bomb))
    emptyCell should be(Cell(visible = false, tagged = false, Empty))
    numberedCell should be(Cell(visible = false, tagged = false, Numbered(n)))
  }
}
