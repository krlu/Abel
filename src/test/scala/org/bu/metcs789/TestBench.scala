package org.bu.metcs789

import org.bu.metcs789.algebraicStructures.rings.polynomials.RealPolynomial

object TestBench {

  def main(args: Array[String]): Unit = {
    val U = RealPolynomial(-16,-24,-4,10,6,1)
    val str = "3^3sdfsd"
    println(('a' to 'z').toList ++ List(""))
    try {
      RealPolynomial.parse(str)
    } catch {
      case e: IllegalArgumentException =>
        println(e.getMessage)
    }
//    val sqFreeFactors = SquareFreeFactorization(U)
//    println(s"$sqFreeFactors")
  }
}

