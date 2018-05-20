package org.bu.metcs789.polynomials

/**
  * Finite polynomial
  *
  * @param coeffs - input coefficients
  */
class Polynomial(coeffs: Double*) extends (Double => Double){

  lazy val coefficients: Seq[Double] = if(coeffs.isEmpty) Seq(0) else if(coeffs.forall(_ == 0)) Seq(0) else coeffs.reverse.dropWhile(_ == 0).reverse
  val leadingCoeff: Double = coefficients.head
  lazy val degree: Int = if(this == Polynomial.zero) 0 else Math.max(0, coefficients.size - 1)
  lazy val factors: Seq[Polynomial] = PolyUtil.kroneckerFactorization(this)
  lazy val isSquareFree: Boolean = factors.size == factors.toSet.size
  lazy val isReducible: Boolean = factors.size > 1
  lazy val derivative = Polynomial(coefficients.indices.map{ i =>coefficients(i) * i}.drop(1):_*)
  lazy val antiDerivative = Polynomial(Array.fill(1)(0.0).toSeq ++ coefficients.indices.map{ i => coefficients(i) * 1.0/(i+1)}:_*)

  def ^ (exp: Int): Polynomial = if(exp == 0) Polynomial(1) else this * (this ^ (exp-1))
  def * (other: Polynomial): Polynomial = {
    coefficients.indices.map{ i =>
      val newCoeffs = Array.fill(i)(0.0).toSeq ++ other.coefficients.map( c => c * coefficients(i))
      Polynomial(newCoeffs:_*)
    }.reduce((p1, p2) => p1 + p2)
  }
  def - (other: Polynomial) = Polynomial(this.coefficients.zipAll(other.coefficients, 0.0, 0.0).map{case(a,b) => a-b}:_*)
  def + (other: Polynomial) = Polynomial(this.coefficients.zipAll(other.coefficients, 0.0, 0.0).map{case(a,b) => a+b}:_*)
  def / (other: Polynomial): (Polynomial, Polynomial) = {
    require(other != Polynomial.zero)
    var quotient = Polynomial.zero
    var remainder = this
    while(remainder.degree >= other.degree) {
      val rLeadCoeff = remainder.coefficients.reverse.head
      val otherLeadCoeff = other.coefficients.reverse.head
      val tempVal = (Polynomial(0, 1) ^ (remainder.degree - other.degree)) * Polynomial(rLeadCoeff / otherLeadCoeff)
      if(tempVal == Polynomial.zero)
        return(quotient, remainder)
      remainder -= tempVal * other
      quotient += tempVal
    }
    (quotient, remainder)
  }
  def % (other: Polynomial): Polynomial = (this/other)._2
  def == (other: Polynomial): Boolean = this.equals(other)
  def != (other: Polynomial): Boolean = !this.equals(other)
  def integral(lowerBound: Double, upperBound: Double): Double = antiDerivative(upperBound) - antiDerivative(lowerBound)

  override def apply(v1: Double): Double = coefficients.indices.map{ i => coefficients(i) * Math.pow(v1, i)}.sum

  override def equals(obj: scala.Any): Boolean = obj match {
    case other: Polynomial =>
      if(this.coefficients.size != other.coefficients.size) false
      else (this.coefficients zip other.coefficients) forall { case (a, b) => a == b }
    case _ => false
  }

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

object test{
  def main(args: Array[String]): Unit = {
    val x = List(1.0)
    val y: Seq[Double] = Array(1.0)
    println(x == y)
  }
}
