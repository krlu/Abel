package org.bu.abel.basics
import org.bu.abel._
import org.bu.abel.algops.rings.IntegerModN

object PrimeUtil{
  /**
    * Using Sieve of Eratosthenes
    */
  def primesLessThanN(n: Long): Seq[Long] = {
    var primes: Seq[Long] = (2 until n.toInt).toList.map(_.toLong)
    for(i <- 2 to Math.floor(Math.sqrt(n.toInt)).toInt){
      if(isPrime(i)){
        for(j <- i*2 to n.toInt by i){
          primes = primes.filter(_ != j)
        }
      }
    }
    primes
  }

  /**
    * The totient function - returns number of integers less than n and relatively prime to n
    */
  def phi(n: Long): Int = relPrimesLessThanN(n).size

  def relPrimesLessThanN(n: Long): Seq[Long] = {
    var primes: Seq[Long] = (1 until n.toInt).toList.map(_.toLong)
    for(i <- 2 to n.toInt){
      if(GCDUtil.gcd(i, n.toInt)._1 != 1)
        primes = primes.filter(_ != i)
    }
    primes
  }

  def isPrime(n: Long): Boolean = {
    if(n < 0) return isPrime(-n)
    if(n == 1) return false
    for(i <- 2 to Math.floor(Math.sqrt(n.toInt)).toInt)
      if(n % i == 0) return false
    true
  }

  /**
    * Probability MillerRabin test is correct
    *
    * @param n - we want to determine if this is prime
    * @param repetitions - probability MillerRabin is correct = Math.pow((1/4),repetitions)
    * @return true if and only if if MillerRabin test deems value is prime
    */
  def millerRabin(n: Long, repetitions: Int): Boolean = {
    require(n > 3 && repetitions < n-3 && repetitions > 0)
    if (n % 2 == 0)
      false
    else {
      val r: Long = factorOut2s(n - 1)
      val m: Long = (n - 1) / Math.pow(2, r).toLong
      var possibleAs = List.range(2, n.toInt - 2)
      for (_ <- 1 to repetitions) {
        val b = choose(possibleAs.iterator)
        possibleAs = possibleAs.filter(_ != b)
        val x = IntegerModN(n).pow(b, m)
        if (x != 1 && x != n - 1) {
          //            println(s"n: $n,b: $b,m: $m,r $r,x: $x")
          if (!millerRabinHelper(x, r, n))
            return false
        }
      }
      true
    }
  }
  private def millerRabinHelper(x: Long, r: Long, n: Long): Boolean = {
    var temp = x
    for (i <- 0 until r.toInt) {
      val exp = Math.pow(2,i).toInt
      temp = IntegerModN(n).pow(x, exp)
      if(temp == 1) return false
      if(temp == n-1) {
        return true
      }
    }
    false
  }

  private def factorOut2s(n: Long): Long ={
    var exp = 0L
    var nTemp = n
    while(nTemp % 2 == 0){
      nTemp = nTemp/2
      exp +=1
    }
    exp
  }
}
