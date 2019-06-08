package org.bu.abel

import org.bu.abel.algebraicStructures.rings.polynomials.RealPolynomial
import org.bu.abel.factorization.polynomial.SquareFreeFactorization

object TestBench {

  def main(args: Array[String]): Unit = {
//    val p1 = RealPolynomial(-27, 117, -90, 90, -48, 8)
    val p1 = RealPolynomial(-110592, 59904, -5760, 720, -48, 1)

    println(SquareFreeFactorization(p1))
  }
}

