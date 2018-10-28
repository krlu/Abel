package org.bu.metcs789.factorization.Integer

object GetAllFactors extends (Long => Seq[Long]){
  def apply(n: Long): Seq[Long] = {
    val primes = PrimeFactorization(Math.abs(n))
    primes.indices.flatMap { i => primes.combinations(i).map(_.product) } ++ Seq(Math.abs(n))
  }
}
