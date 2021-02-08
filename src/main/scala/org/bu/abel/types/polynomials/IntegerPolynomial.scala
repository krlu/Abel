package org.bu.abel.types.polynomials

import org.bu.abel.algops.rings.IntegerRing

class IntegerPolynomial(coeffs: Long*) extends Polynomial[Long, IntegerRing](coeffs:_*)(IntegerRing()){

  lazy val derivative = IntegerPolynomial(coefficients.indices.map{ i => coefficients(i) * i }.drop(1):_*)
  lazy val antiDerivative = IntegerPolynomial(Array.fill(1)(0L).toSeq ++ coefficients.indices.map{ i => coefficients(i) / (i + 1) }:_*)

  def + (other: IntegerPolynomial): IntegerPolynomial = IntegerPolynomial((this add other).coefficients:_*)
  def + (scalar: Long): IntegerPolynomial = IntegerPolynomial((this add scalar).coefficients:_*)
  def - (other: IntegerPolynomial): IntegerPolynomial = IntegerPolynomial((this sub other).coefficients:_*)
  def - (scalar: Long): IntegerPolynomial = IntegerPolynomial((this sub scalar).coefficients:_*)
  def * (other: IntegerPolynomial): IntegerPolynomial = IntegerPolynomial((this mult other).coefficients:_*)
  def * (scalar: Long): IntegerPolynomial = IntegerPolynomial((this scale scalar).coefficients:_*)
  def ^(exp: Long): IntegerPolynomial = IntegerPolynomial((this pow exp).coefficients:_*)
}

object IntegerPolynomial{
  def apply(coefficients: Long*): IntegerPolynomial = new IntegerPolynomial(coefficients:_*)
  def zero = new IntegerPolynomial(0)
  def one = new IntegerPolynomial(1)
}
