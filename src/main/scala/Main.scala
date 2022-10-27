import MineSweeper._
import scala.io.StdIn.readLine

object Main {
  def main(args: Array[String]): Unit = {
    /* Set dependencies */
    val nBombs: Int = 1
    /* Define game machine */
    var gameMachine = MineSweeperAPI((5, 8), nBombs)
    println(gameMachine.grid.mkString)
    println("\n")
    println(gameMachine.pick(2, 2).flatMap(_.tag(3, 2).map(_.grid.mkString)))
    println("\n")
    println(gameMachine.grid.makeVisible.mkString)

    do {
      println(gameMachine.grid.mkString + "\n")

      println("What do you want to do?\n1. Discover a cell\n2. Flag/Unflag a cell")
      val operation = readLine().toInt

      println("X coordinate:")
      val xcoord = readLine().toInt

      println("Y coordinate:")
      val ycoord = readLine().toInt
      gameMachine = if (operation == 1) {
        gameMachine.pick(xcoord, ycoord) match {
          case Left(_) => gameMachine
          case Right(m) => m
        }
      } else gameMachine.tag(xcoord, ycoord) match {
        case Left(_) => gameMachine
        case Right(m) => m
      }

    } while (!gameMachine.isFinished)

    println(gameMachine.grid.mkString)


  }
}