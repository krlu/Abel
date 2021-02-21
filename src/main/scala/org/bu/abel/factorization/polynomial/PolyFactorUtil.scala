package org.bu.abel.factorization.polynomial

import cc.redberry.rings.Integers.Integers
import cc.redberry.rings.bigint.BigInteger
import cc.redberry.rings.poly.PolynomialMethods
import cc.redberry.rings.scaladsl.{UnivariatePolynomial, UnivariateRing}
import org.bu.abel.types.LargeNumber
import org.bu.abel.types.polynomials.RealPolynomial

object PolyFactorUtil {

  def GCD(p1: RealPolynomial, p2: RealPolynomial): RealPolynomial = {
    val p3: UnivariatePolynomial[BigInteger] = toRingsPoly(p1)
    val p4: UnivariatePolynomial[BigInteger] = toRingsPoly(p2)
    var coeffs = List.empty[BigInteger]
    PolynomialMethods.PolynomialGCD(p3, p4).forEach { coeff =>
      coeffs = coeffs ++ List(coeff)
    }
    RealPolynomial.create(coeffs.map(_.intValue().toDouble).map(LargeNumber(_)):_*)
  }

  protected[abel] def toAbelPoly(p: UnivariatePolynomial[BigInteger]): RealPolynomial =
    RealPolynomial.create((0 until p.size()).map(i => LargeNumber(p.get(i).intValue())):_*)

  protected[abel] def toRingsPoly(p: RealPolynomial): UnivariatePolynomial[BigInteger] =
    UnivariateRing(Integers, "x")(p.toString())
}
