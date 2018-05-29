package org.bu.metcs789.algebraicStructures.polynomials

import org.bu.metcs789.RealPoly
import org.bu.metcs789.algebraicStructures.fields.Real
import org.bu.metcs789.factorization.polynomial.Kronecker

/**
  * Finite polynomial with real coefficients
  * @param coeffs - input coefficients
  */
sealed class RealPolynomial(coeffs: Double*) extends Poly[Double, Real](coeffs:_*)(Real()){
  lazy val factors: Seq[RealPoly] = Kronecker(this)
  lazy val isSquareFree: Boolean = factors.size == factors.toSet.size
  lazy val isReducible: Boolean = factors.size > 1
  lazy val derivative = RealPolynomial(coefficients.indices.map{ i =>coefficients(i) * i}.drop(1):_*)
  lazy val antiDerivative = RealPolynomial(Array.fill(1)(0.0).toSeq ++ coefficients.indices.map{ i => coefficients(i) * 1.0/(i+1)}:_*)

  override def toString(): String =
    if(this == Poly.zero) "0.0"
    else {
      coefficients.indices.map { i =>
        val coeffStr = coefficients(i) match {
          case c if c == 0 || (c == 1 && i != 0) => ""
          case c if c < 0 => s"($c)"
          case c => s"$c"
        }
        val expStr = i match {
          case exp if exp == 0 || coefficients(i) == 0 => ""
          case exp if exp == 1 => "x"
          case exp => s"x^$exp"
        }
        s"$coeffStr$expStr"
      }.filter(_.nonEmpty).reverse.mkString(" + ")
    }
}

object RealPolynomial{
  def apply(coefficients: Double*): RealPolynomial = new RealPolynomial(coefficients:_*)
}
