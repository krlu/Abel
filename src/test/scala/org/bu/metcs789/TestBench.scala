package org.bu.metcs789

import org.bu.metcs789.algebraicStructures.rings.polynomials.{PolyUtil, RealPolynomial}

object TestBench {
  def main(args: Array[String]): Unit = {
    val p1 = RealPolynomial(-16, -24, -4, 10, 6, 1)
    val p2 = p1.derivative
//    println(PolyUtil.GCD(p1, p2))
//    println(RealPolynomial(4,4,1)* (RealPolynomial(-6,4,5) + RealPolynomial(2,-6,-3,1)) == p1)
//    println(p1/RealPolynomial(4,4,1))
//    println(p2/RealPolynomial(4,4,1))
    println(PolyUtil.GCD(p1, p2))
  }
}
