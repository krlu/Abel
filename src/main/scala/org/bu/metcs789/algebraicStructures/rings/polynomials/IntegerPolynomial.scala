package org.bu.metcs789.algebraicStructures.rings.polynomials

import org.bu.metcs789.algebraicStructures.rings.Integer

class IntegerPolynomial(coeffs: Int*) extends Polynomial[Int, Integer](coeffs:_*)(Integer()){

  private type IntPoly = Polynomial[Int, Integer]
  
  lazy val derivative = IntegerPolynomial(coefficients.indices.map{ i => coefficients(i) * i }.drop(1):_*)
  lazy val antiDerivative = IntegerPolynomial(Array.fill(1)(0).toSeq ++ coefficients.indices.map{ i => (coefficients(i) * 1.0/(i+1)).toInt}:_*)

  def + (other: IntegerPolynomial): IntegerPolynomial = IntegerPolynomial((this.asInstanceOf[IntPoly] + other.asInstanceOf[IntPoly]).coefficients:_*)
  def - (other: IntegerPolynomial): IntegerPolynomial = IntegerPolynomial((this.asInstanceOf[IntPoly] - other.asInstanceOf[IntPoly]).coefficients:_*)
  def * (other: IntegerPolynomial): IntegerPolynomial = IntegerPolynomial((this.asInstanceOf[IntPoly] * other.asInstanceOf[IntPoly]).coefficients:_*)
  def ^(exp: Int): IntegerPolynomial = IntegerPolynomial((this pow exp).coefficients:_*)
}

object IntegerPolynomial{
  def apply(coefficients: Int*): IntegerPolynomial = new IntegerPolynomial(coefficients:_*)
  def zero = new IntegerPolynomial(0)
  def one = new IntegerPolynomial(1)
}
