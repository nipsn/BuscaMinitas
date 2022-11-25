package MineSweeper

import org.scalatest._
import flatspec._
import matchers._
class CellTests extends AnyFlatSpec with should.Matchers {
  val n = 1
  val bombCell: Cell = Cell(Bomb)
  val emptyCell: Cell = Cell(Empty)
  val numberedCell: Cell = Cell(Numbered(n))

  val numberedCellTwo: Cell = Cell(Numbered(2))
  val numberedCellThree: Cell = Cell(Numbered(3))
  val numberedCellFour: Cell = Cell(Numbered(4))
  val numberedCellFive: Cell = Cell(Numbered(5))
  val numberedCellSix: Cell = Cell(Numbered(6))
  val numberedCellSeven: Cell = Cell(Numbered(7))
  val numberedCellEight: Cell = Cell(Numbered(8))


  "A cell" should "be visible" in {
    bombCell.makeVisible.visible should be(true)
    emptyCell.makeVisible.visible should be(true)
    numberedCell.makeVisible.visible should be(true)

    bombCell.makeVisible.toString should be(Representation.bomb)
    emptyCell.makeVisible.toString should be("_")
    numberedCell.makeVisible.toString should be(Representation.one)

    numberedCellTwo.makeVisible.toString should be(Representation.two)
    numberedCellThree.makeVisible.toString should be(Representation.three)
    numberedCellFour.makeVisible.toString should be(Representation.four)
    numberedCellFive.makeVisible.toString should be(Representation.five)
    numberedCellSix.makeVisible.toString should be(Representation.six)
    numberedCellSeven.makeVisible.toString should be(Representation.seven)
    numberedCellEight.makeVisible.toString should be(Representation.eight)
  }

  "A cell" should "be marked" in {
    emptyCell.changeTag.tagged should be(true)
    emptyCell.changeTag.changeTag.tagged should be(false)

    emptyCell.changeTag.toString should be(Representation.flag)
    emptyCell.changeTag.changeTag.toString should be(" ")
  }

  "A cell" should "be properly constructed" in {
    bombCell should be(Cell(visible = false, tagged = false, Bomb))
    emptyCell should be(Cell(visible = false, tagged = false, Empty))
    numberedCell should be(Cell(visible = false, tagged = false, Numbered(n)))

    val constructorCell: Cell = Cell(visible = true, tagged = false, kind = Empty)

    constructorCell.kind should be(Empty)
    constructorCell.tagged should be(false)
    constructorCell.visible should be(true)
  }
}
