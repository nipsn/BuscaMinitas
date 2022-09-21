package MineSweeper

sealed trait CellKind
case object Empty extends CellKind
case object Bomb extends CellKind
case class Numbered(value: Int) extends CellKind
