package org.bu.metcs789


object GCD extends ((Int, Int) => (Int, List[(Int, Int)])){
  override def apply(m: Int, n: Int): (Int, List[(Int, Int)]) = euclideanAlgo(m, n)
  private def euclideanAlgo(m: Int, n: Int): (Int, List[(Int, Int)]) = {
    if(m%n == 0) return (n, List((m/n, m%n)))
    val(gcd, list) = euclideanAlgo(n, m%n)
    (gcd, (List((m/n, m%n)) ++ list).filter{case (_, b) => b != 0})
  }
}

object ExtendedEuclideanAlgorithm extends ((Int, Int) =>(Int,Int)){
  override def apply(x: Int, y: Int): (Int, Int) = {
    val(_, list) = GCD(x,y)
    val reversedList = list.reverse
    val (b0, _) = reversedList.head
    var A =  1
    var B = -b0
    val remainingList = reversedList.drop(1)
    for(i <- remainingList.indices){
      val (b, _) = remainingList(i)
      A = A + B * (-b) // a = q* b + r
      B = B * 1
      val ATemp = A
      A = B
      B = ATemp
    }
    (A, B)
  }
}
