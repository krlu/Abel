package org.bu.metcs789.factorization

object PrimeFactorization extends (Long => Seq[Long]){
  override def apply(n: Long): Seq[Long] = factorizationHelper(n)
  
  private def factorizationHelper(n: Long): Seq[Long] = {
    val f1 = PollardRho(n)
    if(f1 == 1) Seq(n) else factorizationHelper(f1) ++ factorizationHelper(n/f1)
  }
}


