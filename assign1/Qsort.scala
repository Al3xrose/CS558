// Alex Rose

import scala.util.Random
object Qsort {
  val MINSIZE = 10

  //Swap two array elemtns, a[i] <-> a[j]
  def swap(a: Array[Int], i: Int, j: Int)
  {
    if(i != j)
    {
      val tmp = a(i)
      a(i) = a(j)
      a(j) = tmp
    }
  }

  // Create an array to contain a random permutation of 1..N
  def createArray(N: Int): Array[Int] = {
    var a = new Array[Int](N)
    var j = 0
    var random = new Random()

    for(i <- 0 to N - 1){
      a(i) = i + 1
    }

    for(i <- 0 to N - 1){
      j = random.nextInt(N) 
      swap(a, i, j) 
    }
    a
  }

  // Bubble sort for the base cases
  def bubbleSort(a: Array[Int], low: Int, high: Int): Array[Int] = {
    if(low < high){
      for(i <- low to high) {
        for(j <- i+1 to high){
          if(a(i) > a(j)){
            swap(a, i, j)
          }
        }
      }
    }
    a
  }

  // Partition an array range a[low,high] into two sub-ranges
  def partition(a: Array[Int], low: Int, high: Int): Int = {
    var pivot = a(high)
    var middle = low

    for(i <- low to high-1){
      if(a(i) < pivot){
        swap(a, i, middle)
        middle = middle + 1
      }
    }
    swap(a, high, middle)
    middle
  }

  //QuickSort an array range a[low, high]
  def quickSort(a: Array[Int], low: Int, high: Int){
    if(high - low < MINSIZE){
      bubbleSort(a, low, high)
    }
    else{
      var middle = partition(a, low, high)
      if(low < middle){
        quickSort(a, low, middle-1)
      }
      if(middle < high){
        quickSort(a, middle+1, high)
      }
    }
  }

  //Print an array
  def printArray(a: Array[Int], N: Int){
    for(i <- 0 to N - 1){
      printf("%d, ", a(i))
    }
    printf("\n")
  }

  // Verify a sorted array
  def verifyArray(a: Array[Int], N: Int){
    var failed = 0
    for(i <- 0 to N - 2){
      if(a(i)>a(i+1) && failed == 0){
        printf("FAILED: a(%d)=%d, a(%d)=%d\n",
          i, a(i), i+1, a(i+1))
        failed = 1
      }
    }
    if(failed == 0)
      println("Result verified!")
    
  }

  def main(args: Array[String]) {
    val N = args(0).toInt
    var a = createArray(N)
    
    println("Initial array:")
    printArray(a, N)

    quickSort(a, 0, N-1)

    println("After quickSort:")
    printArray(a, N)
    
    verifyArray(a, N)
  }
}
