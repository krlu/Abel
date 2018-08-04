package org.bu.metcs789

import org.bu.metcs789.algebraicStructures.rings.polynomials.{PolyUtil, RealPolynomial}

object TestBench {
  def main(args: Array[String]): Unit = {
        val p1 = RealPolynomial(-16, -24, -4, 10, 6, 1)
        val p2 = p1.derivative
    //    println(p1)
    //    println(p2)
    //    val (q,r) = p1/p2
    //    println(q * p2 + r)
        println(PolyUtil.GCD(p1, p2))

  }
}
