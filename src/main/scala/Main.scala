object Main {
  def main(args: Array[String]): Unit = {
//    val mymultiarr= Array.ofDim[(Int, Int)](4, 4)
    val mymultiarr= Array.ofDim[Int](8, 8)

    //Assigning the values
    mymultiarr(0)(0) = 2
    mymultiarr(0)(1) = 7
    mymultiarr(1)(0) = 3
    mymultiarr(1)(1) = 4

    for(i<-0 to 1) {
      for(j<-0 to 1) {
        print("[" + mymultiarr(i)(j) + "]")
      }
      println()
    }


    print(mymultiarr.map("[" + _.mkString("][") + "]").mkString("\n"))
  }
}