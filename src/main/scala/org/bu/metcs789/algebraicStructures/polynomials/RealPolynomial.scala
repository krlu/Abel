package org.bu.metcs789.algebraicStructures.polynomials

import org.bu.metcs789.algebraicStructures.fields.Real
import org.bu.metcs789.basics.GCD
import org.bu.metcs789.factorization.polynomial.Kronecker

/**
  * Finite polynomial with real coefficients
  * @param coeffs - input coefficients
  */
sealed class RealPolynomial(coeffs: Double*) extends Polynomial[Double, Real](coeffs:_*)(ring = Real()){

  private type RealPoly = Polynomial[Double, Real]

  lazy val factors: Seq[RealPolynomial] = Kronecker(this)
  lazy val isSquareFree: Boolean = factors.size == factors.toSet.size
  lazy val isReducible: Boolean = factors.size > 1
  lazy val derivative = RealPolynomial(coefficients.indices.map{ i =>coefficients(i) * i}.drop(1):_*)
  lazy val antiDerivative = RealPolynomial(Array.fill(1)(0.0).toSeq ++ coefficients.indices.map{ i => coefficients(i) * 1.0/(i+1)}:_*)
  lazy val nearZero: Boolean = this.coefficients.size == 1 && Math.abs(this.coefficients.head) < 0.000001


  def + (other: RealPolynomial): RealPolynomial = RealPolynomial((this.asInstanceOf[RealPoly] + other.asInstanceOf[RealPoly]).coefficients:_*)
  def - (other: RealPolynomial): RealPolynomial = RealPolynomial((this.asInstanceOf[RealPoly] - other.asInstanceOf[RealPoly]).coefficients:_*)
  def * (other: RealPolynomial): RealPolynomial = RealPolynomial((this.asInstanceOf[RealPoly] * other.asInstanceOf[RealPoly]).coefficients:_*)
  def ^(exp: Int): RealPolynomial = RealPolynomial((this pow exp).coefficients:_*)
  def % (other: RealPolynomial): RealPolynomial = (this/other)._2

  // TODO: this is broken
  def / (other: RealPolynomial): (RealPolynomial, RealPolynomial) = {
    val zeroPoly = RealPolynomial.zero
    require(other != zeroPoly)
    var quotient = zeroPoly
    var remainder = RealPolynomial(coeffs:_*)
    var divisionIndex = 0
    if(this.coefficients.isEmpty) return (other, RealPolynomial(0.0))
    while(remainder.degree >= other.degree && remainder.degree >= 0) {
      var rLeadCoeff = remainder.coefficients.reverse(divisionIndex)
      val otherLeadCoeff = other.coefficients.reverse.head
      while(Math.abs(rLeadCoeff) < Math.abs(otherLeadCoeff)){
        divisionIndex += 1
        if(divisionIndex == remainder.coefficients.size) {
          return (quotient, remainder)
        }
        rLeadCoeff = remainder.coefficients.reverse(divisionIndex)
      }
      val tempVal = (RealPolynomial(ring.zero, ring.one) ^ (remainder.degree - other.degree)) * RealPolynomial(ring.div(rLeadCoeff,otherLeadCoeff))
      if(tempVal == zeroPoly)
        return (quotient, remainder)
      remainder = RealPolynomial((remainder - (tempVal * other)).coefficients:_*)
      quotient += tempVal
    }
    (quotient, remainder)
  }

  private def coeffsGCD(coeffs: Seq[Int]): Int ={
    val nonZeroCoeffs = coeffs.filter(_ > 0)
    if(nonZeroCoeffs.size == 1) nonZeroCoeffs.head
    else if(nonZeroCoeffs.size == 2) GCD(nonZeroCoeffs.head, nonZeroCoeffs(1))._1.toInt
    else {
      val (left, right) = nonZeroCoeffs.splitAt((nonZeroCoeffs.size + 1) / 2)
      GCD(coeffsGCD(left), coeffsGCD(right))._1.toInt
    }
  }
}

object RealPolynomial{
  def apply(coefficients: Double*): RealPolynomial = new RealPolynomial(coefficients:_*)
  def zero = new RealPolynomial(0.0)
  def one = new RealPolynomial(1.0)
}

object foo{
  def main(args: Array[String]): Unit = {
//    val p1 = RealPolynomial(-16, -24, -4, 10, 6, 1)
//    val p2 = p1.derivative
//    println(p1)
//    println(p2)
//    val (q,r) = p1/p2
//    println(q * p2 + r)
//    println(PolyUtil.GCD(p1, p2))
    val p1 = RealPolynomial(-1,0,1)
    val p2 = RealPolynomial(2,2)
    println(p2)
    println(p2/RealPolynomial(2))
//    println(p2/p1)
//    println(PolyUtil.GCD(p1, p2))
  }

}
