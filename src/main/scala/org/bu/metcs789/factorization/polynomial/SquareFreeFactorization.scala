package org.bu.metcs789.factorization.polynomial
import org.bu.metcs789.algebraicStructures.rings.polynomials.{PolyUtil, RealPolynomial}

object SquareFreeFactorization extends PolynomialFactorizationAlgo {
  override def apply(u: RealPolynomial): Seq[RealPolynomial] = {
    if(u == RealPolynomial.zero) Seq(u)
    else{
      var p = RealPolynomial.one
      var r = PolyUtil.GCD(u, u.derivative)
      var f = (u/r)._1
      var j = 1
      while(r != RealPolynomial.one){
        val g = PolyUtil.GCD(r, f)
        val s = (f/g)._1
        p = p * (s ^ j)
        r = (r/g)._1
        f = g
        j += 1
      }
      val moreFactors = if(p == RealPolynomial.one) List() else SquareFreeFactorization(p)
      List.fill(j)(f) ++ moreFactors
    }
  }
}