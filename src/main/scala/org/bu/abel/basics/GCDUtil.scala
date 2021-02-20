package org.bu.abel.basics

import org.bu.abel.types.LargeNumber

object GCDUtil{

  def gcd(m: LargeNumber, n: LargeNumber): (LargeNumber, List[(LargeNumber, LargeNumber)]) = euclideanAlgo2(m, n)

  private def euclideanAlgo2(m: LargeNumber, n: LargeNumber): (LargeNumber, List[(LargeNumber, LargeNumber)]) = {
    if(m%n == 0){
       (n, List((m/n, LargeNumber(0))))
    }else{
      val (gcd, list) = euclideanAlgo2(n, m%n)
      (gcd, (List((LargeNumber.roundDown(m/n), m%n)) ++ list))
    }
  }
  def gcd(m: Long, n: Long): (Long, List[(Long, Long)]) = euclideanAlgo(m, n)
  private def euclideanAlgo(m: Long, n: Long): (Long, List[(Long, Long)]) = {
    if(m%n == 0) return (n, List((m/n, m%n)))
    val(gcd, list) = euclideanAlgo(n, m%n)
    (gcd, (List((m/n, m%n)) ++ list).filter{case (_, b) => b != 0})
  }

  def extendedgcd(x: Long, y: Long): (Long, Long) = {
    val(_, list) = gcd(x,y)
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

  def multigcd(values: Seq[Long]): Option[Long] = {
    if(values.isEmpty) None
    else {
      require(values.forall(_ > 0))
      var currentGCD = values.head
      var remainingValues = values.tail
      while (remainingValues.nonEmpty) {
        currentGCD = gcd(currentGCD, remainingValues.head)._1
        remainingValues = remainingValues.tail
      }
      Some(currentGCD)
    }
  }
}