package org.bu.metcs789.factorization

import org.bu.metcs789.basics.IsPrime

object PrimeFactorization extends (Long => Seq[Long]){
  override def apply(n: Long): Seq[Long] = if(n == 1) Seq(n) else factorizationHelper(n)
  
  private def factorizationHelper(n: Long): Seq[Long] = {
    if(IsPrime(n)) return Seq(n)
    val f1 = PollardRho(n)
    if(f1 == 1) Seq(n) else factorizationHelper(f1) ++ factorizationHelper(n/f1)
  }
}

object GetAllFactors extends (Long => Seq[Long]){
  def apply(n: Long): Seq[Long] = {
    val primes = PrimeFactorization(Math.abs(n))
    primes.indices.flatMap { i => primes.combinations(i).map(_.product) } ++ Seq(Math.abs(n))
  }
}
