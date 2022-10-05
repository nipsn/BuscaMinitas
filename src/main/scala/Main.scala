import MineSweeper._

object Main {
  def main(args: Array[String]): Unit = {
    val initialGrid = (5, 8).initialGrid(20)
    println(initialGrid.mkString)
    println("\n")
    println(initialGrid.pick(2, 2).tag(2, 2).mkString)
    println("\n")
    println(initialGrid.makeVisible.mkString)

  }
}