import MineSweeper._

object Main {
  def main(args: Array[String]): Unit = {
    /* Set dependencies */
    val nBombs: Int = 20
    /* Define game machine */
    val gameMachine = MineSweeperAPI((5, 8), nBombs)
    println(gameMachine.grid.mkString)
    println("\n")
    println(gameMachine.pick(2, 2).flatMap(_.tag(3, 2).map(_.grid.mkString)))
    println("\n")
    println(gameMachine.grid.makeVisible.mkString)

  }
}