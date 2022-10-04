import MineSweeper._

object Main {
  def main(args: Array[String]): Unit = {
    val initialGrid = (5, 5).initialGrid(25)
    println(initialGrid.mkString)
    println("\n")
    println(initialGrid.makeVisible.mkString)

  }
}