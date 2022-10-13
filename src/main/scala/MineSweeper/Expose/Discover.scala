package MineSweeper.Expose

import MineSweeper._

trait Discover {

  implicit class DiscoverImp(grid: Grid) {

    def makeFirstLevelNumberedVisible(adjacents: List[(Cell, (Int, Int))], gridO: Grid): Grid = {
      adjacents.foldRight(gridO)((tuple, myGrid) => tuple._1 match {
        case Cell(_, _, Bomb) => myGrid
        case Cell(_, _, Empty) => myGrid
        case Cell(_, tagged, Numbered(n)) => myGrid.modify(tuple._2)(Cell(visible = true, tagged = tagged, Numbered(n)))
      })
    }

    def discover(coords: (Int, Int)): Grid = {
      val visibleAdjacents = grid.getAdjacents(coords)
        .filter(pair => !pair._1.visible)

      val gridWNumberedVisible = makeFirstLevelNumberedVisible(visibleAdjacents, grid)

      visibleAdjacents.filter(pair => pair._1.kind == Empty)
        .foldRight(gridWNumberedVisible)((tuple, myGrid) => myGrid.modify(tuple._2)(tuple._1.makeVisible).discover(tuple._2))
    }
  }
}
