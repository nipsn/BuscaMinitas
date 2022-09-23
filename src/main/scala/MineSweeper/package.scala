package object MineSweeper {

  type Grid = Array[Array[Cell]]
  /* Grid utilities */
  implicit class EmptyGridFromDim(tuple: (Int, Int)) {
    def emptyGrid: Grid = Array.ofDim[Cell](tuple._1, tuple._2)
      .map(row => row.map(_ => Cell(Empty)))
  }

  /* Matrix operation syntax */
  implicit class MatrixSyntax(grid: Grid) {
    def apply(x: Int, y: Int): Cell = grid(x)(y)
    def modify(x: Int, y: Int)(cell: Cell): Grid = grid.updated(x, grid(x).updated(y, cell))
    def mkString: String = grid.map(row => row.map(cell => cell.toString).mkString("[", "][", "]")).mkString("\n")
  }

}
