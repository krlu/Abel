package org.bu.metcs789

import org.bu.metcs789.algebraicStructures.rings.polynomials.{PolyUtil, RealPolynomial}

object TestBench {
  def main(args: Array[String]): Unit = {
    val p1 = RealPolynomial(-16, -24, -4, 10, 6, 1)
    val p2 = p1.derivative
    println(PolyUtil.GCD(p1, p2))
  }
}
