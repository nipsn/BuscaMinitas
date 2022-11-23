package MineSweeper

sealed trait Error extends Throwable
case object AlreadyExposedCell extends Error
case object ArrayIndexOutOfBounds extends Error
case object NotAnOption extends Error
case class GenericError(e: Throwable) extends Error
