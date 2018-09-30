// Alex Rose
object Qsort {
  def main(args: Array[String]) {
    val N = args(0).toInt
    var randomArray = new Array[String](N)
   
    for(i <- 0 to N-1) {
      randomArray(i) = "Hi"
    }

    randomArray.foreach(println)
  }
}
