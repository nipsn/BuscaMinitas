import MineSweeper._
import UI.{run, runInit}
import cats.effect.unsafe.implicits.global

object Main {
  def main(args: Array[String]): Unit = {
    /* Manually create grid */
    //    val nBombs: Int = 10
    //    val gameMachine = MineSweeperAPI((5, 8), nBombs)

    /* Define game machine */
    val gameMachine = runInit().unsafeRunSync()
    println(gameMachine.grid.makeVisible.mkString + "\n")
    run(gameMachine).unsafeRunSync()


  }
}