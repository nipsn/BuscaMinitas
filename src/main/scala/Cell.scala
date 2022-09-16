sealed abstract class Cell {
  def toString: String
  def pick(): Unit
}

case class Empty() extends Cell {
  override def toString: String = " "
  // recursive search
  override def pick(): Unit = ???
}

case class Numbered(value: Int) extends Cell {
  override def toString: String = this.value.toString
  override def pick(): Unit = ???
}

case class Bomb() extends Cell {
  override def toString: String = "B"
  // end game
  override def pick(): Unit = ???
}