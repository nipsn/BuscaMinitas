// TODO: remove vars
sealed abstract class Cell(var visible: Boolean) {
  def toString: String
  def pick(): Unit
  def isVisible: Boolean = visible

  def setVisible(): Unit = this.visible = true
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