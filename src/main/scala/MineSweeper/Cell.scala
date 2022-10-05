package MineSweeper

case class Cell(visible: Boolean, tagged: Boolean, kind: CellKind) {

  override def toString: String = if (this.tagged) "λ" else {
    this.kind match {
      case Bomb => if (this.visible) "B" else " "
      case Empty => if (this.visible) "_" else " "
      case Numbered(value) => if (this.visible) value.toString else " "
    }
  }

  def makeVisible: Cell = this.copy(visible = true)

  def changeTag: Cell = this.copy(tagged = !this.tagged)

  def prevail: Boolean = this match {
    case Cell(true, tag, Bomb) => tag
    case _ => true
  }
}

case object Cell {

  /* Initial Cell */
  def apply(kindc: CellKind): Cell = Cell(
    visible = false,
    tagged = false,
    kind = kindc
  )

}