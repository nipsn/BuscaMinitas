package MineSweeper.Expose

import MineSweeper._

trait Tag {

  implicit class TagImp(grid: Grid) {

    def tag(x: Int, y: Int): Either[Error, Grid] = {
      if (!grid(x, y).visible) Right(grid.modify(x, y)(grid(x, y).changeTag))
      else Left(AlreadyTaggedCell)
    }

    def tag(coords: (Int, Int)): Either[Error, Grid] = tag(coords._1, coords._2)
  }
}
