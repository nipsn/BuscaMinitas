import MineSweeper._

object Main {
  def main(args: Array[String]): Unit = {
    val initialGrid = (5, 5).initialGrid(2)
    println(initialGrid.mkString)
    println("\n")
    println(initialGrid.pick(2,2).mkString)

  }
}