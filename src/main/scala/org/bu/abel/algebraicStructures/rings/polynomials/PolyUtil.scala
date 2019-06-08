package org.bu.abel.algebraicStructures.rings.polynomials

import cc.redberry.rings.Integers.Integers
import cc.redberry.rings.bigint.BigInteger
import cc.redberry.rings.poly.PolynomialMethods
import cc.redberry.rings.scaladsl.{UnivariatePolynomial, UnivariateRing}

object PolyUtil {
  def GCD(p1: RealPolynomial, p2: RealPolynomial): RealPolynomial = {
    val p3: UnivariatePolynomial[BigInteger] = UnivariateRing(Integers, "x")(p1.toString())
    val p4: UnivariatePolynomial[BigInteger] = UnivariateRing(Integers, "x")(p2.toString())
    var coeffs = List.empty[BigInteger]
    PolynomialMethods.PolynomialGCD(p3, p4).forEach { coeff =>
      coeffs = coeffs ++ List(coeff)
    }
    RealPolynomial(coeffs.map(_.intValue().toDouble):_*)
  }

}
