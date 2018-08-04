package org.bu.metcs789.algebraicStructures.rings.polynomials

object PolyUtil {
  def GCD(p1: RealPolynomial, p2: RealPolynomial): RealPolynomial = {
    println("**********************************************************")
//    Thread.sleep(1000)
    if(p2 == RealPolynomial.zero || p2 == RealPolynomial.one) p1
    else {
      val newP1 = helper(p1)
      val newP2 = helper(p2)
      println("hi")
      GCD(newP2, newP1 % newP2)
    }
  }
  private def helper(p: RealPolynomial): RealPolynomial = (if(p.coefficients.reverse.head < 0) p * -1 else p).reduceCoeffs._1

}
