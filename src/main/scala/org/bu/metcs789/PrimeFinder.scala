package org.bu.metcs789

/**
  * Using Sieve of Eratosthenes
  */
object PrimesLessThanN extends (Long => Seq[Long]){
  override def apply(n: Long): Seq[Long] = {
    var primes: Seq[Long] = (2 until n.toInt).toList.map(_.toLong)
    for(i <- 2 to Math.floor(Math.sqrt(n.toInt)).toInt){
      if(IsPrime(i)){
        for(j <- i*2 to n.toInt by i){
          primes = primes.filter(_ != j)
        }
      }
    }
    primes
  }
}

object Totient extends (Long => Int){
  override def apply(n: Long): Int = RelPrimesLessThanN(n).size
}

object RelPrimesLessThanN extends (Long => Seq[Long]){
  override def apply(n: Long): Seq[Long] = {
    var primes: Seq[Long] = (1 until n.toInt).toList.map(_.toLong)
    for(i <- 2 to Math.floor(Math.sqrt(n.toInt)).toInt){
      if(GCD(i, n.toInt)._1 != 1){
        for(j <- i to n.toInt by i){
          primes = primes.filter(_ != j)
        }
      }
    }
    primes
  }
}

object IsPrime extends (Long => Boolean){
  override def apply(n: Long): Boolean = {
    if(n == 1) return false
    for(i <- 2 to Math.floor(Math.sqrt(n.toInt)).toInt)
      if(n % i == 0) return false
    true
  }
}
