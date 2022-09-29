import MineSweeper._

object Main {
  def main(args: Array[String]): Unit = {

    val grid: Grid = (5,5).emptyGrid

    println(grid.mkString)
    println("\n")

    val gridTransformed = grid.map(row =>
      row.map(_ => Cell(Numbered(2))))
      .modify(0, 1)(Cell(Numbered(3)))
      .modify(1, 1)(Cell(Numbered(3)))
      .modify(2, 1)(Cell(Numbered(3)))
      .modify(3, 1)(Cell(Numbered(3)))
      .modify(4, 1)(Cell(Numbered(3)))
      .modify(0, 0)(Cell(Empty))
      .modify(1, 0)(Cell(Empty))
      .modify(2, 0)(Cell(Empty))
      .modify(3, 0)(Cell(Empty))
      .modify(4, 0)(Cell(Empty))
      .modify(4, 1)(Cell(Empty))
      .modify(4, 2)(Cell(Empty))
      .modify(4, 3)(Cell(Empty))
      .modify(4, 4)(Cell(Empty))
      .pick(2, 0)
      .pick(1, 2)

    println(gridTransformed.mkString)

  }
}