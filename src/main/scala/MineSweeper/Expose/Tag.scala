package MineSweeper.Expose

import MineSweeper._

trait Tag {

  implicit class TagImp(grid: Grid) {

    def tag(x: Int, y: Int): Grid = {
      grid.modify(x, y)(grid(x, y).changeTag)
    }

  }
}
