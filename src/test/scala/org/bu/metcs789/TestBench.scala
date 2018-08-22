package org.bu.metcs789

import org.bu.metcs789.algebraicStructures.rings.polynomials.RealPolynomial
import org.bu.metcs789.factorization.polynomial.NewtonsMethod

object TestBench {
  //TODO: problem seems to happen due to different poly degrees and one having larger leading coeff
  def main(args: Array[String]): Unit = {
    val p = RealPolynomial(5,1)^12
    //  val p1 = RealPolynomial(-16, -24, -4, 10, 6, 1)
    //  val t1 = System.currentTimeMillis()
      val factors = NewtonsMethod()(p)
//    val factors = Kronecker(p)
    println(factors)
//    val t2 = System.currentTime Millis()
//    println(t2 - t1)
  }
}

