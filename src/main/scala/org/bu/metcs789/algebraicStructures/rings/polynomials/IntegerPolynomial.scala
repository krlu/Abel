package org.bu.metcs789.algebraicStructures.rings.polynomials

import org.bu.metcs789.algebraicStructures.rings.Integer

class IntegerPolynomial(coeffs: Int*) extends Polynomial[Int, Integer](coeffs:_*)(Integer()){

  lazy val derivative = IntegerPolynomial(coefficients.indices.map{ i => coefficients(i) * i }.drop(1):_*)
  lazy val antiDerivative =
    IntegerPolynomial(Array.fill(1)(0).toSeq ++ coefficients.indices.map{ i => coefficients(i) / (i + 1) }:_*)

  def + (other: IntegerPolynomial): IntegerPolynomial = IntegerPolynomial((this add other).coefficients:_*)
  def + (scalar: Int): IntegerPolynomial = IntegerPolynomial((this add scalar).coefficients:_*)
  def - (other: IntegerPolynomial): IntegerPolynomial = IntegerPolynomial((this sub other).coefficients:_*)
  def - (scalar: Int): IntegerPolynomial = IntegerPolynomial((this sub scalar).coefficients:_*)
  def * (other: IntegerPolynomial): IntegerPolynomial = IntegerPolynomial((this mult other).coefficients:_*)
  def * (scalar: Int): IntegerPolynomial = IntegerPolynomial((this scale scalar).coefficients:_*)
  def ^(exp: Int): IntegerPolynomial = IntegerPolynomial((this pow exp).coefficients:_*)
}

object IntegerPolynomial{
  def apply(coefficients: Int*): IntegerPolynomial = new IntegerPolynomial(coefficients:_*)
  def zero = new IntegerPolynomial(0)
  def one = new IntegerPolynomial(1)
}
