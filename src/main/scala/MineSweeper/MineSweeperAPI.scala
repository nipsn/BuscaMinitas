package MineSweeper

import scala.util.Try
import Expose._
import cats.syntax.all._

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

  def showResult: String = grid.makeVisible.mkString

  def makePick(x: Int, y: Int): MineSweeperAPI = {
    /* Safe computation */
    grid(x)(y) match {
      case Cell(_, _, Empty) => MineSweeperAPI(grid.discover2(x, y))
      case _ => MineSweeperAPI(grid.makeVisible((x, y)))
    }
  }
  def makeTag(x: Int, y: Int): MineSweeperAPI = {
    /* Safe computation */
    MineSweeperAPI(this.grid.tag(x, y))
  }

  def manageErrorOrExecute(coords: (Int, Int), f: (Int, Int) => MineSweeperAPI): Either[Error, MineSweeperAPI] = {
    val (x, y) = coords
    Try(this.grid(x, y))
      .toEither.left.map {
      case _: ArrayIndexOutOfBoundsException => ArrayIndexOutOfBounds
      case e: Throwable => GenericError(e)
    } >>= {
      case Cell(true, _, _) => Left(AlreadyExposedCell)
      case _ => Right(f(x, y))
    }
  }

  def pick(coords: (Int, Int)): Either[Error, MineSweeperAPI] = {
    manageErrorOrExecute(coords, (x, y) => this.makePick(x, y))
  }

  def tag(coords: (Int, Int)): Either[Error, MineSweeperAPI] = {
    manageErrorOrExecute(coords, (x, y) => this.makeTag(x, y))
  }

}

object MineSweeperAPI{
  /* Initial grid with random bomb placement */
  def apply(size: (Int, Int), difficulty: Int, seed: Option[Int]): MineSweeperAPI = MineSweeperAPI(grid = {
    val (height, length) = size
    val totalCells = height * length
    val nBombs = if (difficulty == 1) {
      (totalCells * 0.1).toInt
    } else if (difficulty == 2) {
      (totalCells * 0.15).toInt
    } else {
      (totalCells * 0.2).toInt
    }
    size.initialGrid(nBombs, seed)
  })

}

