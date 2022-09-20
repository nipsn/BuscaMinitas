class Grid(sizeX: Int, sizeY: Int) {
  private val grid: Array[Array[Cell]] = Array.ofDim[Cell](sizeX, sizeY)
  
  // puesto a buco. añadir lógica
  grid(0)(0) = Empty()
  grid(0)(1) = Numbered(3)
  grid(0)(2) = Numbered(2)
  grid(0)(3) = Numbered(2)
  grid(0)(4) = Numbered(2)

  grid(1)(0) = Empty()
  grid(1)(1) = Numbered(2)
  grid(1)(2) = Numbered(2)
  grid(1)(3) = Numbered(2)
  grid(1)(4) = Numbered(2)

  grid(2)(0) = Empty()
  grid(2)(1) = Numbered(2)
  grid(2)(2) = Numbered(2)
  grid(2)(3) = Numbered(2)
  grid(2)(4) = Numbered(2)

  grid(3)(0) = Empty()
  grid(3)(1) = Numbered(2)
  grid(3)(2) = Numbered(2)
  grid(3)(3) = Numbered(2)
  grid(3)(4) = Numbered(2)

  grid(4)(0) = Empty()
  grid(4)(1) = Numbered(2)
  grid(4)(2) = Numbered(2)
  grid(4)(3) = Numbered(2)
  grid(4)(4) = Numbered(2)
  
  override def toString: String = grid.map("[" + _.mkString("][") + "]").mkString("\n")

  def pick(coordX:Int, coordY:Int): Unit = grid(coordX)(coordY).pick()

}
