package org.bu.abel.types.polynomials

import org.bu.abel.algops.fields.Rational
import org.bu.abel.types.Q

class RationalPolynomial(coeffs: Q*) extends OrderedPolynomial[Q, Rational](coeffs:_*)(field = Rational()){

  lazy val derivative = RationalPolynomial(coefficients.indices.map{ i => coefficients(i)*i}.drop(1):_*)
  lazy val antiDerivative =
    RationalPolynomial(Array.fill(1)(field.zero).toSeq ++ coefficients.indices.map{ i => coefficients(i)/(i+1)}:_*)

  def + (other: RationalPolynomial): RationalPolynomial = RationalPolynomial((this add other).coefficients:_*)
  def + (scalar: Q): RationalPolynomial = RationalPolynomial((this add scalar).coefficients:_*)
  def - (other: RationalPolynomial): RationalPolynomial = RationalPolynomial((this sub other).coefficients:_*)
  def - (scalar: Q): RationalPolynomial = RationalPolynomial((this sub scalar).coefficients:_*)
  def * (other: RationalPolynomial): RationalPolynomial = RationalPolynomial((this mult other).coefficients:_*)
  def * (scalar: Q): RationalPolynomial = RationalPolynomial((this scale scalar).coefficients:_*)
  def ^(exp: Int): RationalPolynomial = RationalPolynomial((this pow exp).coefficients:_*)
}

object RationalPolynomial{
  def apply(coefficients: Q*): RationalPolynomial = new RationalPolynomial(coefficients:_*)
  def zero = new RationalPolynomial(Q(0,1))
  def one = new RationalPolynomial(Q(1,1))
}
