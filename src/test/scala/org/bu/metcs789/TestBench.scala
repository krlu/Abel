package org.bu.metcs789

import org.bu.metcs789.algebraicStructures.rings.polynomials.RealPolynomial
import org.bu.metcs789.factorization.polynomial.{NewtonsMethod, SquareFreeFactorization}

object TestBench {

  def main(args: Array[String]): Unit = {
    val U = (RealPolynomial(1,1,1)^10) * (RealPolynomial(1,2)^5)
    val t1 = System.currentTimeMillis()
    val newtonFactors = NewtonsMethod()(U)
    val t2 = System.currentTimeMillis()
    println(s"newtons runtime: ${(t2-t1)/1000.0}, $newtonFactors")
    val sqFreeFactors = SquareFreeFactorization(U)
    val t3 = System.currentTimeMillis()
    println(s"newtons runtime: ${(t3-t2)/1000.0}, $sqFreeFactors")
  }
}

