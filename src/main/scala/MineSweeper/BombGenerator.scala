package MineSweeper

import scala.annotation.tailrec

trait BombGenerator {
  implicit class BombGenerator(grid: Grid) {

    def numerateCell(coords: (Int, Int)): Grid = {
      val nBombs = grid.getAdjacents(coords)
        .count(pair => pair._1.kind == Bomb)
      val newCell = if (nBombs == 0) Cell(Empty) else Cell(Numbered(nBombs))
      grid.modify(coords)(newCell)
    }

    def numerateGrid(size: (Int, Int)): Grid = {
      {
        for {
          x <- 0 to size._1
          y <- 0 to size._2
          if grid.valid(x, y) && grid(x, y).kind != Bomb
        } yield (x, y)
      }.foldRight(grid)((pair, myGrid) => myGrid.numerateCell(pair))

    }
  }

  def generateRandomBombs(gridSize: (Int, Int), nBombs: Int): List[(Int, Int)] = {
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
