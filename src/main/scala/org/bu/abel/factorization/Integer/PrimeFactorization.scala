package org.bu.abel.factorization.Integer

import org.bu.abel.basics.PrimeUtil

object PrimeFactorization extends (Long => Seq[Long]){
  override def apply(n: Long): Seq[Long] = if(n == 1) Seq(n) else factorizationHelper(n)
  
  private def factorizationHelper(n: Long): Seq[Long] = {
    if(PrimeUtil.isPrime(n)) return Seq(n)
    val f1 = PollardRho(n)
    if(f1 == 1) Seq(n) else factorizationHelper(f1) ++ factorizationHelper(n/f1)
  }
}

