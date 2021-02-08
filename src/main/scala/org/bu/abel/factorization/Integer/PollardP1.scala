package org.bu.abel.factorization.Integer

import org.bu.abel._
import org.bu.abel.algops.rings.{IntegerModN, IntegerRing}
import org.bu.abel.basics._

object PollardP1 extends ((Long, Long, Int) => Long){
  private var beta = 1L
  private val Z = IntegerRing()
  override def apply(n: Long, initBeta: Long, maxAttempts: Int = 10): Long = {
    var g: Long = 1
    val zModN = IntegerModN(n)
    beta = initBeta
    var attempts = 0
    var lowerBound = 0L
    var upperBound = Long.MaxValue
    while((g >= n || g <= 1) && attempts <= maxAttempts){
      val M: Long = PrimesLessThanN(beta).map{ q => Z.pow(q,largestExpLessThan(q,n))}.product
      val a = choose(RelPrimesLessThanN(n).iterator)
      g = GCD(zModN.pow(a, M)-1, n)._1
      if(g >= n)
        upperBound = Math.min(upperBound, beta)
      if(g <= 1)
        lowerBound = Math.max(lowerBound, beta)
      beta = if(upperBound == Long.MaxValue) beta*2 else (upperBound + lowerBound)/2
      attempts += 1
    }
    if(attempts > maxAttempts) 1 else g
  }

  /**
    * @param base - q
    * @param value - n
    * @return floor(log_q(n))
    */
  private def largestExpLessThan(base: Long, value: Long): Long = {
    var exp = 0
    var currentValue = Math.pow(base, 0).toLong
    while((currentValue * base) < value){
      exp += 1
      currentValue = currentValue * base
    }
    exp
  }
}
