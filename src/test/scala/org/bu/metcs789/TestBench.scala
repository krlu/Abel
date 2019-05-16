package org.bu.metcs789

import cc.redberry.rings.Integers.Integers
import cc.redberry.rings.bigint.BigInteger
import cc.redberry.rings.scaladsl.{UnivariatePolynomial, UnivariateRing}
import org.bu.metcs789.algebraicStructures.rings.polynomials.RealPolynomial

object TestBench {

  def main(args: Array[String]): Unit = {
    // construct some polynomials


//    val x = RealPolynomial(5,1)
//    val p = x^12
    val p1 = RealPolynomial(-16, -24, -4, 10, -1, 1)
    println(p1)
    val p2: UnivariatePolynomial[BigInteger] = UnivariateRing(Integers, "x")(p1.toString())
    println(p2)
//    val t1 = System.currentTimeMillis()
//    val factors = NewtonsMethod()(p)
//    val factors = Kronecker(p)
//    println(factors)
//    val t2 = System.currentTime Millis()
//    println(t2 - t1)
  }
}

