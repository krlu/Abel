package org.bu.abel.types.polynomials

import cc.redberry.rings.Integers.Integers
import cc.redberry.rings.bigint.BigInteger
import cc.redberry.rings.poly.PolynomialMethods
import cc.redberry.rings.scaladsl.{UnivariatePolynomial, UnivariateRing}

object PolyUtil {

  def GCD(p1: RealPolynomial, p2: RealPolynomial): RealPolynomial = {
    val p3: UnivariatePolynomial[BigInteger] = toRingsPoly(p1)
    val p4: UnivariatePolynomial[BigInteger] = toRingsPoly(p2)
    var coeffs = List.empty[BigInteger]
    PolynomialMethods.PolynomialGCD(p3, p4).forEach { coeff =>
      coeffs = coeffs ++ List(coeff)
    }
    RealPolynomial(coeffs.map(_.intValue().toDouble).map(BigDecimal(_)):_*)
  }

  def factorRealPolynomial(p: RealPolynomial): Set[RealPolynomial] = {
    val ringsP = toRingsPoly(p)
    val x = PolynomialMethods.Factor(ringsP)
    (0 until x.size()).map(x.get).map (toAbelPoly).toSet
  }

  protected[abel] def toAbelPoly(p: UnivariatePolynomial[BigInteger]): RealPolynomial =
    RealPolynomial((0 until p.size()).map(i => BigDecimal(p.get(i).intValue())):_*)

  protected[abel] def toRingsPoly(p: RealPolynomial): UnivariatePolynomial[BigInteger] =
    UnivariateRing(Integers, "x")(p.toString())
}
