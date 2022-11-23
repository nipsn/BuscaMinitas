
package object MineSweeper extends BombGenerator {

  type Grid = Array[Array[Cell]]

  /* Grid utilities */
  implicit class GridFromDim(size: (Int, Int)) {

    def emptyGrid: Grid = Array.ofDim[Cell](size._1, size._2)
      .map(row => row.map(_ => Cell(Empty)))

    def initialGrid(nBombs: Int): Grid = {
      emptyGrid
        .generateRandomBombs(nBombs)
        .numerateGrid
    }
  }

  /* Matrix operation syntax */
  implicit class MatrixSyntax(grid: Grid) {

    def apply(x: Int, y: Int): Cell = grid(x)(y)

    def apply(coords: (Int, Int)): Cell = grid(coords._1)(coords._2)

    def modify(x: Int, y: Int)(cell: Cell): Grid = grid.updated(x, grid(x).updated(y, cell))

    def modify(coords: (Int, Int))(cell: Cell): Grid = {
      modify(coords._1, coords._2)(cell)
    }

    def valid(coords: (Int, Int)): Boolean =
      coords._1 >= 0 && coords._1 < grid.length &&
        coords._2 >= 0 && coords._2 < grid(0).length
  }

  implicit class MatrixTransformations(grid: Grid) {

    def makeVisible: Grid = {
      grid.map(_.map(_.makeVisible))
    }

    def makeVisible(coords: (Int, Int)): Grid = grid.modify(coords)(grid(coords).makeVisible)

    def mkString: String = grid.map(row => row.map(cell => cell.toString).mkString("[", "][", "]")).mkString("\n")

    def getAdjacents(coords: (Int, Int)): List[(Cell, (Int, Int))] = {
      for {
        x <- (-1 to 1).map(_ + coords._1)
        y <- (-1 to 1).map(_ + coords._2)
        if (x, y) != coords && grid.valid(x, y)
      } yield (grid(x, y), (x, y))
    }.toList
  }

}
