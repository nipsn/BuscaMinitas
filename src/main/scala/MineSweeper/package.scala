package object MineSweeper {

  type Grid = Array[Array[Cell]]
  /* Grid utilities */
  implicit class EmptyGridFromDim(tuple: (Int, Int)) {
    def emptyGrid: Grid = Array.ofDim[Cell](tuple._1, tuple._2)
      .map(row => row.map(_ => Cell(Empty)))

    def emptyGridWithBombs(nBombs: Int): Grid = {
      generateRandomBombs(tuple, nBombs)
        .foldRight(emptyGrid)((coords, myGrid) => myGrid.modify(coords)(Cell(visible = true, tagged = false, Bomb)))
    }

    private def generateRandomBombs(gridSize: (Int, Int), nBombs: Int): List[(Int, Int)] = {
      val (coordX, coordY) = gridSize
      val r = scala.util.Random
      // TODO: bombs can generate in same coords multiple times (check nBombs == set.size)
      (1 to nBombs).foldRight(Set[(Int, Int)]())((_, mySet) => mySet.incl((r.nextInt(coordX), r.nextInt(coordY)))).toList
    }
  }

  /* Matrix operation syntax */
  implicit class MatrixSyntax(grid: Grid) {
    def apply(x: Int, y: Int): Cell = grid(x)(y)

    def modify(x: Int, y: Int)(cell: Cell): Grid = grid.updated(x, grid(x).updated(y, cell))

    def modify(coords: (Int, Int))(cell: Cell): Grid = {
      val (x, y) = coords
      grid.updated(x, grid(x).updated(y, cell))
    }
    def mkString: String = grid.map(row => row.map(cell => cell.toString).mkString("[", "][", "]")).mkString("\n")
  }

}
