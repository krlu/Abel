package org.bu.metcs789.polynomials

import org.bu.metcs789.factorization.PrimeFactorization

object PolyUtil {
  def kroneckerFactorization(p: Polynomial): Set[Polynomial] = {
    val p0 = p(0)
    val p1 = p(1)
    val p2 = p(2)
    ???
  }

  def berlekampFactorization(p: Polynomial): Set[Polynomial] = ???

  def GCD(p1: Polynomial, p2: Polynomial): Polynomial = {
    if(p2 == Polynomial.zero) p1
    else {
      GCD(p2, p1 % p2)
    }
  }
}

object foo{
  def main(args: Array[String]): Unit = {
    println(PrimeFactorization(24))
  }
}
