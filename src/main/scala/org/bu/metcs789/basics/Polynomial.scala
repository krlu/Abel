package org.bu.metcs789.basics


protected class Polynomial(val input: Double*) extends (Double => Double){

  val coefficients: Seq[Double] = if(input.isEmpty) Seq(0) else input

  override def apply(v1: Double): Double = coefficients.indices.map{ i => coefficients(i) * Math.pow(v1, i)}.sum
  override def toString(): String = {
    coefficients.indices.map{ i =>
      s"(${coefficients(i)})*x^$i"
    }.reverse.mkString(" + ")
  }

  def simpleExpression: String = coefficients.indices.map { i =>
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
      val multStr = if(coeffStr.nonEmpty && expStr.nonEmpty) "*" else ""
      s"$coeffStr$multStr$expStr"
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
  def - (other: Polynomial) = Polynomial(other.coefficients.zipAll(this.coefficients, 0.0, 0.0).map{case(a,b) => a-b}:_*)
  def + (p: Polynomial) = Polynomial(p.coefficients.zipAll(this.coefficients, 0.0, 0.0).map{case(a,b) => a+b}:_*)

  def derivative: Polynomial = Polynomial(coefficients.indices.map{ i =>coefficients(i) * i}.drop(1):_*)
  def antiDerivative: Polynomial = Polynomial(Array.fill(1)(0.0).toSeq ++ coefficients.indices.map{ i =>
    coefficients(i) * (if(i == 0) 1 else 1.0/i)
  }:_*)

}

object Polynomial{
  def apply(coefficients: Double*): Polynomial = new Polynomial(coefficients:_*)
}

object Test{
  def main(args: Array[String]): Unit = {
    val p1 = Polynomial(1, 1)
    println(p1.simpleExpression)
    val p2 = p1^2
    println(p2.simpleExpression)
    println(p2.derivative.simpleExpression)
    println(p2.derivative.antiDerivative.simpleExpression)
  }
}
