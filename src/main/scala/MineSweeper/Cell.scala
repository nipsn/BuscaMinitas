package MineSweeper

// TODO: remove vars
/*
sealed abstract class Cell(val visible: Boolean) {
  def toString: String
  def pick(): Unit
  def isVisible: Boolean = visible

  def setVisible(): Cell = this.copy(visible=true)
}

case class Empty() extends Cell(visible = false) {
  override def toString: String = if (super.isVisible) "_" else " "
  // recursive search
  override def pick(): Unit = super.setVisible()
}

case class Numbered(value: Int) extends Cell(visible = false) {
  override def toString: String = if (super.isVisible) this.value.toString else " "
  override def pick(): Unit = super.setVisible()
}

case class Bomb(marked: Boolean) extends Cell(visible = false) {
  override def toString: String = if (this.marked) "λ" else " "
  // end game
  override def pick(): Unit = super.setVisible()
}
*/
////////////
case class Cell(visible: Boolean, tagged: Boolean, kind: CellKind) {

  def makeVisible(cell: Cell): Cell = this.copy(visible=true)
  def tag(cell: Cell): Cell = this.copy(tagged = true)
}

case object Cell {

  /* Initial Cell */
  def apply(kindc: CellKind): Cell = Cell(
    visible = false,
    tagged = false,
    kind = kindc
  )

}