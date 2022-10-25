package MineSweeper.Expose

import MineSweeper._

trait Discover {

  implicit class DiscoverImp(grid: Grid) {
    def discover(coords: (Int, Int)): Grid = {
      grid.getAdjacents(coords)
        .filter { case (cell, _) => !cell.visible }
        .foldLeft(grid) { case (myGrid, (cell, coords)) =>
          val gridModified = myGrid.modify(coords)(cell.makeVisible)
          cell.kind match {
            case Empty => gridModified.discover(coords)
            case _ => gridModified
          }
        }
    }

    //    def discover(coords: (Int, Int)): Grid = {
    //      grid.getAdjacents(coords)
    //        .filter { case (cell, _) => !cell.visible }
    //        .foldLeft(grid) { case (myGrid, (cell, coords)) =>
    //          val gridModified = myGrid.modify(coords)(cell.makeVisible)
    //          cell.kind match {
    //            case Empty => gridModified.discover(coords)
    //            case _ => gridModified
    //          }
    //        }
    //    }
  }
}
