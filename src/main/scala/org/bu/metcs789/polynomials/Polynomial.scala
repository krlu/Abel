package org.bu.metcs789.polynomials

import org.bu.metcs789.RealPoly

/**
  * Finite polynomial with real coefficients
 *
  * @param coeffs - input coefficients
  */
class Polynomial(coeffs: Double*) extends Poly[Double, Real](coeffs:_*)(Real()){
  lazy val factors: Seq[RealPoly] = PolyUtil.kroneckerFactorization(this)
  lazy val isSquareFree: Boolean = factors.size == factors.toSet.size
  lazy val isReducible: Boolean = factors.size > 1
  lazy val derivative = Polynomial(coefficients.indices.map{ i =>coefficients(i) * i}.drop(1):_*)
  lazy val antiDerivative = Polynomial(Array.fill(1)(0.0).toSeq ++ coefficients.indices.map{ i => coefficients(i) * 1.0/(i+1)}:_*)

  override def toString(): String =
    if(this == Polynomial.zero) "0.0"
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

object Polynomial{
  def apply(coefficients: Double*): Polynomial = new Polynomial(coefficients:_*)
  val zero = new Polynomial(0)
  val one = new Polynomial(1)
}

/**
  * Generalization of a Polynomial
  * @param coeffs - coefficients for polynomial
  * @param field - Algebraic Field that governs set T of values
  * @tparam T - Set of values coefficients can take
  * @tparam U - Type bound on Algebraic Field
  */
class Poly[T, U <: Field[T]](coeffs: T*)(field: U) extends (T => T){

  lazy val coefficients: Seq[T] = if(coeffs.isEmpty || coeffs.forall(field.eq(_, field.zero))) Seq(field.zero) else coeffs.reverse.dropWhile(field.eq(_, field.zero)).reverse
  lazy val degree: Int = Math.max(0, coefficients.size - 1)
  val leadingCoeff: T = coefficients.head

  def + (other: Poly[T, U]): Poly[T, U] = Poly[T, U](this.coefficients.zipAll(other.coefficients, field.zero, field.zero).map{case(a,b) => field.add(a,b)}:_*)(field)
  def - (other: Poly[T, U]): Poly[T, U] = Poly[T, U](this.coefficients.zipAll(other.coefficients, field.zero, field.zero).map{case(a,b) => field.sub(a,b)}:_*)(field)
  def ^ (exp: Int): Poly[T, U] = if(exp == 0) Poly[T, U](field.one)(field) else this * (this ^ (exp-1))
  def * (other: Poly[T, U]): Poly[T, U] = {
    if(this.coefficients.isEmpty || other.coefficients.isEmpty) return Poly[T, U](field.zero)(field)
    coefficients.indices.map{ i =>
      val newCoeffs = (0 until i).map(_ => field.zero) ++ other.coefficients.map(c => field.mult(c,coefficients(i)))
      Poly[T, U](newCoeffs:_*)(field)
    }.reduce((p1, p2) => p1 + p2)
  }
  def / (other: Poly[T, U]): (Poly[T, U], Poly[T, U]) = {
    val zeroPoly = Poly[T, U](field.zero)(field)
    require(other != zeroPoly)
    var quotient = zeroPoly
    var remainder = this
    if(this.coefficients.isEmpty) return (other, Poly[T, U](field.zero)(field))
    while(remainder.degree >= other.degree && remainder.degree > 0) {
      val rLeadCoeff = remainder.coefficients.reverse.head
      val otherLeadCoeff = other.coefficients.reverse.head
      val tempVal = (Poly[T, U](field.zero, field.one)(field) ^ (remainder.degree - other.degree)) * Poly[T, U](field.div(rLeadCoeff,otherLeadCoeff))(field)
      if(tempVal == zeroPoly)
        return (quotient, remainder)
      remainder -= tempVal * other
      quotient += tempVal
    }
    (quotient, remainder)
  }

  def % (other: Poly[T, U]): Poly[T,U] = (this/other)._2
  def == (other: Poly[T, U]): Boolean = this.equals(other)
  def != (other: Poly[T, U]): Boolean = !this.equals(other)

  override def apply(v1: T): T = coefficients.indices.map{ i => field.mult(coefficients(i),field.pow(v1, i))}.reduce((a, b) => field.add(a, b))
  override def equals(obj: scala.Any): Boolean = obj match {
    case other: Poly[T,U] =>
      if(this.coefficients.size != other.coefficients.size) false
      else (this.coefficients zip other.coefficients) forall { case (a, b) => a == b }
    case _ => false
  }

  def compose(other: T => T): Poly[T, U] = {
    other match {
      case poly: Poly[T, U] =>
        poly.coefficients.indices.map { i =>
          val c = poly.coefficients(i)
          Poly[T,U](c)(field) * (this ^ i)
        }.reduce((p1, p2) => p1 + p2)
      case _ => throw new IllegalArgumentException("input is not a polynomial!!")
    }
  }
}

object Poly{
  def apply[T, U <: Field[T]](coeffs: T*)(field: U): Poly[T,U] = new Poly[T, U](coeffs:_*)(field)
  def apply(coeffs: Double*) = new Poly[Double, Real](coeffs:_*)(Real())
  def zero = new Poly[Double, Real](0)(Real())
  def one = new Poly[Double, Real](1)(Real())
}

