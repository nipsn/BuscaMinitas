import MineSweeper._
import UI.run
import cats.effect.unsafe.implicits.global

object Main {
  def main(args: Array[String]): Unit = {
    /* Set dependencies */
    val nBombs: Int = 1
    /* Define game machine */
    val gameMachine = MineSweeperAPI((5, 8), nBombs)
    //    println(gameMachine.grid.mkString)
    //    println("\n")
    //    println(gameMachine.pick(2, 2).flatMap(_.tag(3, 2).map(_.grid.mkString)))
    //    println("\n")
    println(gameMachine.grid.makeVisible.mkString + "\n")

    run(gameMachine).unsafeRunSync()


  }
}