package org.bu.metcs789


object GCD extends ((Long, Long) => (Long, List[(Long, Long)])){
  override def apply(m: Long, n: Long): (Long, List[(Long, Long)]) = euclideanAlgo(m, n)
  private def euclideanAlgo(m: Long, n: Long): (Long, List[(Long, Long)]) = {
    if(m%n == 0) return (n, List((m/n, m%n)))
    val(gcd, list) = euclideanAlgo(n, m%n)
    (gcd, (List((m/n, m%n)) ++ list).filter{case (_, b) => b != 0})
  }
}

object ExtendedGCD extends ((Long, Long) =>(Long,Long)){
  override def apply(x: Long, y: Long): (Long, Long) = {
    val(_, list) = GCD(x,y)
    val reversedList = list.reverse
    val (b0, _) = reversedList.head
    var A = 1.toLong
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
