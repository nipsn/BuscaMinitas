object Main {
  def main(args: Array[String]): Unit = {
    val grid = new Grid(5,5)
    println(grid)
    grid.pick(0, 1)
    grid.pick(0, 0)
    println(grid)

  }
}