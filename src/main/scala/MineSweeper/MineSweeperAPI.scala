package MineSweeper

import scala.util.Try

case class MineSweeperAPI(grid: Grid) {

  /* State of the competition */
  def alive: Boolean = this.grid.flatten.forall(_.prevail)
  def lost: Boolean = !this.alive
  def won: Boolean = this.grid.flatten.forall{
    case Cell(false, tagged, Bomb) => tagged
    case Cell(true, _, Empty) => true
    case Cell(true, _, Numbered(_)) => true
    case _ => false
  }
  def isFinished: Boolean = this.lost || this.won

  def makeExpose: MineSweeperAPI = ??? //safe computation (TO DO)

  /* manage arrayindexoutofboundsexception error too (maybe utility) */
  def takeExpose(tuple: (Int, Int)): Either[Error, MineSweeperAPI] = {
    Try(this.grid(tuple._1, tuple._2))
      .toEither.left.map{
      case _: ArrayIndexOutOfBoundsException => ArrayIndexOutOfBounds
      case e: Throwable => GenericError(e)
    }.flatMap{
      case Cell(true, _, _) => Left(AlreadyExposedCell)
      case _ => Right(this.makeExpose)
    }
  }

}

object MineSweeperAPI{
  def apply(size: Int): MineSweeperAPI = MineSweeperAPI(grid = (size, size).emptyGrid)
}

