package org.bu.metcs789.algebraicStructures.rings.polynomials

import org.bu.metcs789.algebraicStructures.fields.Rational


class RationalPolynomial(coeffs: (Int, Int)*) extends Polynomial[(Int, Int), Rational](coeffs:_*)(ring = Rational()){

  private type RationalPoly = Polynomial[(Int, Int), Rational]

  lazy val derivative = RationalPolynomial(coefficients.indices.map{ i =>
    val (a, b) = coefficients(i)
    (a*i, b)
  }.drop(1):_*)
  lazy val antiDerivative = RationalPolynomial(Array.fill(1)(ring.zero).toSeq ++ coefficients.indices.map{ i =>
    val (a, b) = coefficients(i)
    (a, b * (i + 1))
  }:_*)

  def + (other: RationalPolynomial): RationalPolynomial = RationalPolynomial((this.asInstanceOf[RationalPoly] + other.asInstanceOf[RationalPoly]).coefficients:_*)
  def - (other: RationalPolynomial): RationalPolynomial = RationalPolynomial((this.asInstanceOf[RationalPoly] - other.asInstanceOf[RationalPoly]).coefficients:_*)
  def * (other: RationalPolynomial): RationalPolynomial = RationalPolynomial((this.asInstanceOf[RationalPoly] * other.asInstanceOf[RationalPoly]).coefficients:_*)
  def ^(exp: Int): RationalPolynomial = RationalPolynomial((this pow exp).coefficients:_*)
}

object RationalPolynomial{
  def apply(coefficients: (Int, Int)*): RationalPolynomial = new RationalPolynomial(coefficients:_*)
  def zero = new RationalPolynomial((0, 1))
  def one = new RationalPolynomial((1,1))
}
