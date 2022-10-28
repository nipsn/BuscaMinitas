package MineSweeper

case class Cell(visible: Boolean, tagged: Boolean, kind: CellKind) {

  override def toString: String = if (this.tagged) Representation.flag else {
    this.kind match {
      case Bomb => if (this.visible) Representation.bomb else " "
      case Empty => if (this.visible) "_" else " "
      case Numbered(value) => if (this.visible) {
        value match {
          case 1 => Representation.one
          case 2 => Representation.two
          case 3 => Representation.three
          case 4 => Representation.four
          case 5 => Representation.five
          case 6 => Representation.six
          case 7 => Representation.seven
          case 8 => Representation.eight
        }
      } else " "
    }
  }

  def makeVisible: Cell = this.copy(visible = true)

  def changeTag: Cell = this.copy(tagged = !this.tagged)

  def prevail: Boolean = this match {
    case Cell(true, tag, Bomb) => tag
    case _ => true
  }
}

object Cell {
  /* Initial Cell */
  def apply(kindc: CellKind): Cell = Cell(
    visible = false,
    tagged = false,
    kind = kindc
  )
}