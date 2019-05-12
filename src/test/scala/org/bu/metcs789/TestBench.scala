package org.bu.metcs789

import org.bu.metcs789.algebraicStructures.rings.polynomials.RealPolynomial

object TestBench {
  //TODO: problem seems to happen due to different poly degrees and one having larger leading coeff
  def main(args: Array[String]): Unit = {
//    val x = RealPolynomial(5,1)
//    val p = x^12
    val x1 = Seq(RealPolynomial(3,1), RealPolynomial(5,1), RealPolynomial(1,1), RealPolynomial(4,1), RealPolynomial(7,1))
    val x2 = Seq(RealPolynomial(3,1), RealPolynomial(5,1), RealPolynomial(1,1), RealPolynomial(4,1), RealPolynomial(7,1))
    println(x1.forall(x2.contains))
    //  val p1 = RealPolynomial(-16, -24, -4, 10, 6, 1)
    //  val t1 = System.currentTimeMillis()
//      val factors = NewtonsMethod()(p)
//    val factors = Kronecker(p)
//    println(factors)
//    val t2 = System.currentTime Millis()
//    println(t2 - t1)
  }
}

