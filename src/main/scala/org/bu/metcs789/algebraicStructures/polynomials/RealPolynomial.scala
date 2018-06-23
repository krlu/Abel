package org.bu.metcs789.algebraicStructures.polynomials

import org.bu.metcs789.algebraicStructures.fields.Real
import org.bu.metcs789.factorization.polynomial.Kronecker

/**
  * Finite polynomial with real coefficients
  * @param coeffs - input coefficients
  */
sealed class RealPolynomial(coeffs: Double*) extends Polynomial[Double, Real](coeffs:_*)(Real()){

  private type RealPoly = Polynomial[Double, Real]

  lazy val factors: Seq[RealPolynomial] = Kronecker(this)
  lazy val isSquareFree: Boolean = factors.size == factors.toSet.size
  lazy val isReducible: Boolean = factors.size > 1
  lazy val derivative = RealPolynomial(coefficients.indices.map{ i =>coefficients(i) * i}.drop(1):_*)
  lazy val antiDerivative = RealPolynomial(Array.fill(1)(0.0).toSeq ++ coefficients.indices.map{ i => coefficients(i) * 1.0/(i+1)}:_*)

  def + (other: RealPolynomial): RealPolynomial = RealPolynomial((this.asInstanceOf[RealPoly] + other.asInstanceOf[RealPoly]).coefficients:_*)
  def - (other: RealPolynomial): RealPolynomial = RealPolynomial((this.asInstanceOf[RealPoly] - other.asInstanceOf[RealPoly]).coefficients:_*)
  def * (other: RealPolynomial): RealPolynomial = RealPolynomial((this.asInstanceOf[RealPoly] * other.asInstanceOf[RealPoly]).coefficients:_*)
  def ^(exp: Int): RealPolynomial = RealPolynomial((this pow exp).coefficients:_*)
  def % (other: RealPolynomial): RealPolynomial = (this/other)._2
  def / (other: RealPolynomial): (RealPolynomial, RealPolynomial) = {
    val zeroPoly = RealPolynomial.zero
    require(other != zeroPoly)
    var quotient = zeroPoly
    var remainder = RealPolynomial(coeffs:_*)
    if(this.coefficients.isEmpty) return (other, RealPolynomial(0.0))
    while(remainder.degree >= other.degree && remainder.degree > 0) {
      val rLeadCoeff = remainder.coefficients.reverse.head
      val otherLeadCoeff = other.coefficients.reverse.head
      val tempVal = (RealPolynomial(ring.zero, ring.one) ^ (remainder.degree - other.degree)) * RealPolynomial(ring.div(rLeadCoeff,otherLeadCoeff))
      if(tempVal == zeroPoly)
        return (quotient, remainder)
      remainder = RealPolynomial((remainder - (tempVal * other)).coefficients:_*)
      quotient += tempVal
    }
    (quotient, remainder)
  }
}

object RealPolynomial{
  def apply(coefficients: Double*): RealPolynomial = new RealPolynomial(coefficients:_*)
  def zero = new RealPolynomial(0.0)
  def one = new RealPolynomial(1.0)
}
