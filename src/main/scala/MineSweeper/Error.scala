package MineSweeper

trait Error extends Throwable
case object AlreadyExposedCell extends Error
case object AlreadyTaggedCell extends Error
case object ArrayIndexOutOfBounds extends Error
case class GenericError(e: Throwable) extends Error
