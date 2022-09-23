import MineSweeper._

object Main {
  def main(args: Array[String]): Unit = {

    val grid: Grid = (5,5).emptyGrid

    println(grid.mkString)
    println("\n")

    val gridTransformed = grid.map(row =>
      row.map(_ =>Cell(Numbered(2))))
      .modify(0,1)(Cell(Numbered(3)).makeVisible)
      .modify(0,0)(Cell(Empty).makeVisible)
      .modify(1,0)(Cell(Empty).makeVisible)
      .modify(2,0)(Cell(Empty))
      .modify(3,0)(Cell(Empty))
      .modify(4,0)(Cell(Empty))

    println(gridTransformed.mkString)

  }
}