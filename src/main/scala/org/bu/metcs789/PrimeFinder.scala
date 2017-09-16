package org.bu.metcs789

/**
  * Using Sieve of Eratosthenes
  */
object PrimesLessThanN extends (Int => Seq[Int]){
  override def apply(n: Int): Seq[Int] = {
    var primes: Seq[Int] = (2 until n).toList
    for(i <- 2 to Math.floor(Math.sqrt(n)).toInt){
      if(IsPrime(i)){
        for(j <- i*2 to n by i){
          primes = primes.filter(_ != j)
        }
      }
    }
    primes
  }
}

object RelPrimesLessThanN extends (Int => Seq[Int]){
  override def apply(n: Int): Seq[Int] = {
    var primes: Seq[Int] = (1 until n).toList
    for(i <- 2 to Math.floor(Math.sqrt(n)).toInt){
      if(GCD(i, n)._1 != 1){
        for(j <- i to n by i){
          primes = primes.filter(_ != j)
        }
      }
    }
    primes
  }
}

object IsPrime extends (Int => Boolean){
  override def apply(n: Int): Boolean = {
    if(n == 1) return false
    for(i <- 2 to Math.floor(Math.sqrt(n)).toInt)
      if(n % i == 0) return false
    true
  }
}

object test2{
  def main(args: Array[String]): Unit = {
    println(RelPrimesLessThanN(9))
  }
}
