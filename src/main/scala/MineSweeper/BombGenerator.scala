package MineSweeper

import scala.annotation.tailrec

trait BombGenerator {
  implicit class BombGenerator(grid: Grid) {

    val gridH: Int = grid.length
    val gridD: Int = grid.head.length

    def numerateGrid: Grid = {
      {
        for {
          x <- 0 to gridH
          y <- 0 to gridD
          if grid.valid(x, y) && grid(x, y).kind != Bomb
        } yield (x, y)
      }.foldLeft(grid)((myGrid, pair) => myGrid.numerateCell(pair))
    }

    def numerateCell(coords: (Int, Int)): Grid = {
      val nBombs = grid.getAdjacents(coords)
        .count { case (cell, _) => cell.kind == Bomb }
      val newCell = if (nBombs == 0) Cell(Empty) else Cell(Numbered(nBombs))
      grid.modify(coords)(newCell)
    }

    def generateRandomBombs(nBombs: Int): Grid = {
      // TODO: exception when nBombs > gridSize (x * y)
      val r = scala.util.Random

      @tailrec
      def genAux(auxSet: Set[(Int, Int)], elem: (Int, Int)): Set[(Int, Int)] = {
        if (auxSet.size < nBombs) {
          val newSet = if (auxSet.contains(elem)) auxSet else auxSet.incl((r.nextInt(gridH), r.nextInt(gridD)))
          genAux(newSet, (r.nextInt(gridH), r.nextInt(gridD)))
        } else {
          auxSet
        }
      }

      genAux(Set[(Int, Int)](), (r.nextInt(gridH), r.nextInt(gridD))).toList
        .foldLeft(grid)((myGrid, coords) => myGrid.modify(coords)(Cell(Bomb)))
    }
  }
}
