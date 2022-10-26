package MineSweeper.Expose

import MineSweeper._
import cats.data.State
import cats.data.State._
import cats.implicits._

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

    def discover2(coords: (Int, Int)): Grid = {
      discoverWState(coords).runS(grid).value
    }

    private def discoverWState(coords: (Int, Int)): State[Grid, Unit] = {
      grid.getAdjacents(coords)
        .filter { case (cell, _) => !cell.visible }
        .traverse {
          case (cell, coords) => modify[Grid] {
            gridS => {
              val gridModified = gridS.modify(coords)(cell.makeVisible)
              cell.kind match {
                case Empty => gridModified.discover2(coords)
                case _ => gridModified
              }
            }
          }
        }.void
    }

  }
}
