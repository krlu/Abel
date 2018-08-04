package org.bu.metcs789.algebraicStructures.rings.polynomials

object PolyUtil {
  def GCD(p1: RealPolynomial, p2: RealPolynomial): RealPolynomial = {
//    println(p1 , p2)
//    Thread.sleep(1000)
    if(p2 == RealPolynomial.zero || p2 == RealPolynomial.one) p1
    else {
//      if(p1 == p1%p2) return RealPolynomial.one
      GCD(p2, p1 % p2)
    }
  }
}
