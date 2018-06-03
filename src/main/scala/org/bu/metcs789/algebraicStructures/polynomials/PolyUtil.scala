package org.bu.metcs789.algebraicStructures.polynomials

object PolyUtil {
  //  def berlekampFactorization(p: Polynomial): Set[Polynomial] = ???
  def GCD(p1: RealPolynomial, p2: RealPolynomial): RealPolynomial = if(p2 == RealPolynomial.zero) p1 else GCD(p2, p1 % p2)
}
