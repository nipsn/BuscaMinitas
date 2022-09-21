package MineSweeper

class Grid(sizeX: Int, sizeY: Int) {
  /* Constructor: Initial grid from first pick (TO DO LAST) */
  private val grid: Array[Array[Cell]] = Array.ofDim[Cell](sizeX, sizeY)
  
  // puesto a buco. añadir lógica
  grid(0)(0) = Cell(Empty)
  grid(0)(1) = Cell(Numbered(3))
  grid(0)(2) = Cell(Numbered(2))
  grid(0)(3) = Cell(Numbered(2))
  grid(0)(4) = Cell(Numbered(2))

  grid(1)(0) = Cell(Empty)
  grid(1)(1) = Cell(Numbered(2))
  grid(1)(2) = Cell(Numbered(2))
  grid(1)(3) = Cell(Numbered(2))
  grid(1)(4) = Cell(Numbered(2))

  grid(2)(0) = Cell(Empty)
  grid(2)(1) = Cell(Numbered(2))
  grid(2)(2) = Cell(Numbered(2))
  grid(2)(3) = Cell(Numbered(2))
  grid(2)(4) = Cell(Numbered(2))

  grid(3)(0) = Cell(Empty)
  grid(3)(1) = Cell(Numbered(2))
  grid(3)(2) = Cell(Numbered(2))
  grid(3)(3) = Cell(Numbered(2))
  grid(3)(4) = Cell(Numbered(2))

  grid(4)(0) = Cell(Empty)
  grid(4)(1) = Cell(Numbered(2))
  grid(4)(2) = Cell(Numbered(2))
  grid(4)(3) = Cell(Numbered(2))
  grid(4)(4) = Cell(Numbered(2))
  
  override def toString: String = grid.map("[" + _.mkString("][") + "]").mkString("\n")

  /* discover */
  def pick(coordX:Int, coordY:Int): Unit = ???

}
