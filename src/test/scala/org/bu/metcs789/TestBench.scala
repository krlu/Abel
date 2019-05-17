package org.bu.metcs789

import org.bu.metcs789.algebraicStructures.rings.polynomials.RealPolynomial
import org.bu.metcs789.factorization.polynomial.SquareFreeFactorization

object TestBench {

  def main(args: Array[String]): Unit = {
    val U = RealPolynomial(-16,-24,-4,10,6,1)
    val str = "x^5 + 6x^4 + 10x^3 - 4x^2 - 24x - 16"
    println(RealPolynomial.parse(str))
    val sqFreeFactors = SquareFreeFactorization(U)
    println(s"$sqFreeFactors")
  }
}

