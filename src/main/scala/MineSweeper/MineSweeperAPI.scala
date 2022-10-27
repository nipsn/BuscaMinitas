package MineSweeper

import scala.util.Try
import Expose._

case class MineSweeperAPI(grid: Grid) extends Tag {

  /* State of the competition */
  def alive: Boolean = this.grid.flatten.forall(_.prevail)

  def lost: Boolean = !this.alive

  def won: Boolean = this.grid.flatten.forall {
    case Cell(false, tagged, Bomb) => tagged
    case Cell(true, _, Empty) => true
    case Cell(true, _, Numbered(_)) => true
    case _ => false
  }

  def isFinished: Boolean = this.lost || this.won

  def mkString: String = grid.mkString

  def makePick(x: Int, y: Int): MineSweeperAPI = {
    /* Safe computation */
    grid(x)(y) match {
      case Cell(_, _, Empty) => MineSweeperAPI(grid.discover2(x, y))
      case _ => MineSweeperAPI(grid.makeVisible((x, y)))
    }
  }

  def pick(tuple: (Int, Int)): Either[Error, MineSweeperAPI] = {
    Try(this.grid(tuple))
      .toEither.left.map{
      case _: ArrayIndexOutOfBoundsException => ArrayIndexOutOfBounds
      case e: Throwable => GenericError(e)
    }.flatMap{
      case Cell(true, _, _) => Left(AlreadyExposedCell)
      case Cell(_, true, _) => Left(AlreadyTaggedCell)
      case _ => Right(this.makePick(tuple._1, tuple._2))
    }
  }

  def tag(tuple: (Int, Int)): Either[Error, MineSweeperAPI] = {
    this.grid.tag(tuple).map(grid => MineSweeperAPI(grid))
  }

}

object MineSweeperAPI{

  /* Empty squared Grid */
  def apply(side: Int): MineSweeperAPI = MineSweeperAPI(grid = (side, side).emptyGrid)

  /* Initial grid with random bomb placement */
  def apply(size: (Int, Int), nBombs: Int): MineSweeperAPI = MineSweeperAPI(grid = size.initialGrid(nBombs))

}

