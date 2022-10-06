import scala.annotation.tailrec

package object MineSweeper {

  private def coordsAreValid(coords: (Int, Int), grid: Grid): Boolean = coords._1 >= 0 && coords._1 < grid.length && coords._2 >= 0 && coords._2 < grid(0).length

  type Grid = Array[Array[Cell]]

  /* Grid utilities */
  implicit class EmptyGridFromDim(size: (Int, Int)) {
    def initialGrid(nBombs: Int): Grid = {
      val gridWBombs = generateRandomBombs(size, nBombs)
        .foldRight(emptyGrid)((coords, myGrid) => myGrid.modify(coords)(Cell(Bomb)))

      numerateGrid(size, gridWBombs)
    }

    def emptyGrid: Grid = Array.ofDim[Cell](size._1, size._2)
      .map(row => row.map(_ => Cell(Empty)))

    private def numerateGrid(size: (Int, Int), grid: Grid): Grid = {
      val (gridLen, gridWid) = size
      val xCoords = 0 to gridLen
      val yCoords = 0 to gridWid
      xCoords.flatMap(x => yCoords.map(y => (x, y))
        .filter(t => coordsAreValid(t, grid) && grid(t._1, t._2).kind != Bomb))
        .foldRight(grid)((pair, myGrid) => myGrid.numerateCell(pair))
    }

    private def generateRandomBombs(gridSize: (Int, Int), nBombs: Int): List[(Int, Int)] = {
      // TODO: exception when nBombs > gridSize (x * y)
      val (coordX, coordY) = gridSize
      val r = scala.util.Random

      @tailrec
      def genAux(auxSet: Set[(Int, Int)], elem: (Int, Int)): Set[(Int, Int)] = {
        if (auxSet.size < nBombs) {
          val newSet = if (auxSet.contains(elem)) auxSet else auxSet.incl((r.nextInt(coordX), r.nextInt(coordY)))
          genAux(newSet, (r.nextInt(coordX), r.nextInt(coordY)))
        } else {
          auxSet
        }
      }

      genAux(Set[(Int, Int)](), (r.nextInt(coordX), r.nextInt(coordY))).toList
    }
  }

  /* Matrix operation syntax */
  implicit class MatrixSyntax(grid: Grid) {
    def apply(x: Int, y: Int): Cell = grid(x)(y)

    def modify(x: Int, y: Int)(cell: Cell): Grid = grid.updated(x, grid(x).updated(y, cell))

    def modify(coords: (Int, Int))(cell: Cell): Grid = {
      val (x, y) = coords
      modify(x, y)(cell)
    }

    def tag(x: Int, y: Int): Grid = {
      // TODO: add game logic to end game if all bombs are flagged
      val cell = grid(x, y)
      if (!cell.visible) modify(x, y)(cell.changeTag)
      else {
        // TODO: functional exception or smth idk
        println("CAN'T DO DAT M8")
        grid
      }
    }

    def tag(coords: (Int, Int)): Grid = {
      val (x, y) = coords
      tag(x, y)
    }

    def mkString: String = grid.map(row => row.map(cell => cell.toString).mkString("[", "][", "]")).mkString("\n")

    def pick(x: Int, y: Int): Grid = {
      grid(x)(y) match {
        case Cell(_, tagged, Bomb) => grid.modify(x, y)(Cell(visible = true, tagged = tagged, Bomb))
        case Cell(_, _, Empty) => discover((x, y), grid)
        case Cell(_, tagged, Numbered(n)) => grid.modify(x, y)(Cell(visible = true, tagged = tagged, Numbered(n)))
      }
    }

    def makeVisible: Grid = {
      val xCoords = grid.indices
      val yCoords = grid.head.indices
      xCoords.flatMap(x => yCoords.map(y => ((x, y), grid(x, y))))
        .foldRight(grid)((pair, myGrid) => myGrid.modify(pair._1)(pair._2.makeVisible))
    }

    def numerateCell(coords: (Int, Int)): Grid = {
      val nBombs = getAdjacents(coords, grid)
        .count(pair => pair._1.kind == Bomb)
      val newCell = if (nBombs == 0) Cell(Empty) else Cell(Numbered(nBombs))
      grid.modify(coords)(newCell)
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
      val xCoords = (-1 to 1).map(coordX + _)
      val yCoords = (-1 to 1).map(coordY + _)
      xCoords.flatMap(x => yCoords.map(y => (x, y))
        .filter(t => t != (coordX, coordY) && coordsAreValid(t, grid))
        .map(t => (grid(t._1)(t._2), t))).toList
    }

  }

}
