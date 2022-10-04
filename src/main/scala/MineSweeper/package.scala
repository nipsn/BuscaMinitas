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

    def pick(x: Int, y: Int): Grid = {
      grid(x)(y) match {
        case Cell(_, tagged, Bomb) => grid.modify(x, y)(Cell(visible = true,tagged = tagged,Bomb))
        case Cell(_, _, Empty) => discover((x, y), grid)
        case Cell(_, tagged, Numbered(n)) => grid.modify(x, y)(Cell(visible = true, tagged = tagged, Numbered(n)))
      }
    }

    private def makeFirstLevelNumberedVisible(adjacents: List[(Cell, (Int, Int))], gridO: Grid): Grid = {
      adjacents.foldRight(gridO)((tuple, myGrid) => tuple._1 match {
        case Cell(_, _, Bomb) => myGrid
        case Cell(_, _, Empty) => myGrid
        case Cell(_, tagged, Numbered(n)) => myGrid.modify(tuple._2)(Cell(visible = true, tagged = tagged, Numbered(n)))
      })
    }
    private def discover(coords: (Int, Int), gridO: Grid): Grid = {
      val visibleAdjacents = getAdjacents(coords, gridO)
        .filter(pair => !pair._1.visible)

      val gridWNumberedVisible = makeFirstLevelNumberedVisible(visibleAdjacents, gridO)

      visibleAdjacents.filter(pair => pair._1.kind == Empty)
        .foldRight(gridWNumberedVisible)((tuple, myGrid) => discover(tuple._2, myGrid.modify(tuple._2)(tuple._1.makeVisible)))
    }

    private def getAdjacents(coords: (Int, Int), grid: Grid): List[(Cell, (Int, Int))] = {
      val (coordX, coordY) = coords
      val xCoords = (-1 to 1).toList.map(coordX + _)
      val yCoords = (-1 to 1).toList.map(coordY + _)
      xCoords.flatMap(x => yCoords.map(y => (x, y))
        .filter(t => t != (coordX, coordY) && coordsAreValid(t))
        .map(t => (grid(t._1)(t._2), t)))
    }

    private def coordsAreValid(coords: (Int, Int)): Boolean = coords._1 >= 0 && coords._1 < grid.length && coords._2 >= 0 && coords._2 < grid(0).length
  }

}
