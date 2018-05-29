package org.bu.metcs789.algebraicStructures.polynomials

import org.bu.metcs789._

object PolyUtil {
  //  def berlekampFactorization(p: Polynomial): Set[Polynomial] = ???
  def GCD(p1: RealPoly, p2: RealPoly): RealPoly = if(p2 == Poly.zero) p1 else GCD(p2, p1 % p2)
}
