package org.bu.metcs789.basics


protected class Polynomial(val input: Double*) extends (Double => Double){

  val coefficients: Seq[Double] = if(input.isEmpty) Seq(0) else input

  override def apply(v1: Double): Double = coefficients.indices.map{ i => coefficients(i) * Math.pow(v1, i)}.sum
  override def toString(): String = coefficients.indices.map { i =>
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

  def ^ (exp: Int): Polynomial = expHelper(this, exp)
  private def expHelper(base: Polynomial, exp: Int): Polynomial = {
    require(exp >= 0)
    if(exp == 0) Polynomial(1) else base * expHelper( base, exp - 1)
  }
  
  def * (other: Polynomial): Polynomial = {
    coefficients.indices.map{ i =>
      val newCoeffs = Array.fill(i)(0.0).toSeq ++ other.coefficients.map( c => c * coefficients(i))
      Polynomial(newCoeffs:_*)
    }.reduce((p1, p2) => p1 + p2)
  }
  
  def - (other: Polynomial) = Polynomial(this.coefficients.zipAll(other.coefficients, 0.0, 0.0).map{case(a,b) => a-b}:_*)
  def + (other: Polynomial) = Polynomial(this.coefficients.zipAll(other.coefficients, 0.0, 0.0).map{case(a,b) => a+b}:_*)
  def derivative: Polynomial = Polynomial(coefficients.indices.map{ i =>coefficients(i) * i}.drop(1):_*)
  def antiDerivative: Polynomial = Polynomial(Array.fill(1)(0.0).toSeq ++ coefficients.indices.map{ i =>
    coefficients(i) * 1.0/(i+1)
  }:_*)

  /**
    * Factor's a polynomial into a set of irreducible polynomials whose product equals this polynomial
    * If this polynomial is irreducible, this function returns a singleton set containing this polynomial
    * @return Seq[Polynomial]
    */
  def factor: Set[Polynomial] = ???
  def isIrreducible: Boolean = factor.size == 1
}

object Polynomial{
  def apply(coefficients: Double*): Polynomial = new Polynomial(coefficients:_*)
}
