package org.bu.metcs789

import org.bu.metcs789.algebraicStructures.rings.polynomials.RealPolynomial
import org.bu.metcs789.factorization.polynomial.NewtonsMethod

object TestBench {
  //TODO: problem seems to happen due to different poly degrees and one having larger leading coeff
  def main(args: Array[String]): Unit = {
    val p = RealPolynomial(1,1)^9
//    val p1 = RealPolynomial(-16, -24, -4, 10, 6, 1)
    val t1 = System.currentTimeMillis()
    val factors = NewtonsMethod()(p)
    val t2 = System.currentTimeMillis()
    println(factors)
    println(t2 - t1)
  }
}

