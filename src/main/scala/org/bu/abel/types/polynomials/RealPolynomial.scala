package org.bu.abel.types.polynomials

import org.bu.abel._
import org.bu.abel.algops.fields.Real
import org.bu.abel.basics.GCDUtil
import org.bu.abel.factorization.polynomial.Kronecker
import org.bu.abel.types.LargeNumber

/**
  * Finite polynomial with real coefficients
  * Supports division and modular arithmetic
  * @param coeffs - input coefficients
  */
sealed class RealPolynomial(coeffs: LargeNumber*) extends Polynomial[LargeNumber, Real](coeffs:_*)(ring = Real()){

  lazy val factors: Seq[RealPolynomial] = Kronecker(this)
  lazy val isSquareFree: Boolean = factors.size == factors.toSet.size
  lazy val isReducible: Boolean = factors.size > 1
  lazy val derivative: RealPolynomial = RealPolynomial.create(coefficients.indices.map{ i =>coefficients(i) * i}.drop(1):_*)
  lazy val antiDerivative: RealPolynomial = RealPolynomial.create(Seq(LargeNumber(0.0)) ++ coefficients.indices.map{ i => coefficients(i) * 1.0/(i+1)}:_*)

  def apply(input: Double): LargeNumber = this.apply(LargeNumber(input))

  // wrappers for the arithmetic functions from Polynomial superclass
  def + (other: RealPolynomial): RealPolynomial = RealPolynomial.create((this add other).coefficients:_*)
  def + (scalar: LargeNumber): RealPolynomial = RealPolynomial.create((this add scalar).coefficients:_*)
  def - (other: RealPolynomial): RealPolynomial = RealPolynomial.create((this sub other).coefficients:_*)
  def - (scalar:LargeNumber): RealPolynomial = RealPolynomial.create((this sub scalar).coefficients:_*)
  def * (other: RealPolynomial): RealPolynomial = RealPolynomial.create((this mult other).coefficients: _*)

  def * (scalar: LargeNumber): RealPolynomial = RealPolynomial.create((this scale scalar).coefficients:_*)
  def * (other: Double): RealPolynomial = this * LargeNumber(other)
  def ^(exp: Int): RealPolynomial = RealPolynomial.create((this pow exp).coefficients:_*)

  def unary_- : RealPolynomial = RealPolynomial.create(this.invert.coefficients:_*)

  // functions supporting division
  def % (other: RealPolynomial): RealPolynomial = (this/other)._2
  def / (other: RealPolynomial): (RealPolynomial, RealPolynomial) = {
    val zeroPoly = RealPolynomial.zero
    require(other != zeroPoly)
    var quotient = zeroPoly
    var remainder = RealPolynomial.create(coeffs:_*)
    if(this.coefficients.isEmpty) return (other, zeroPoly)
    while(remainder.degree >= other.degree) {
      var divisionIndex = 0
      var rLeadCoeff = remainder.coefficients.reverse(divisionIndex)
      val otherLeadCoeff = other.leadingCoeff
      // inner while loop to enforce integer division
      while(rLeadCoeff.abs < otherLeadCoeff.abs){
        divisionIndex += 1
        if(divisionIndex + other.degree > remainder.degree)
          return (quotient, remainder)
        rLeadCoeff = remainder.coefficients.reverse(divisionIndex)
      }
      val tempVal = (RealPolynomial.create(ring.zero, ring.one) ^ (remainder.degree - divisionIndex - other.degree))*
        RealPolynomial.create(ring.div(rLeadCoeff - (rLeadCoeff % otherLeadCoeff),otherLeadCoeff))
      if(tempVal == zeroPoly)
        return (quotient, remainder)
      remainder = RealPolynomial.create((remainder - (tempVal * other)).coefficients:_*)
      quotient = RealPolynomial.create((quotient + tempVal).coefficients:_*)
    }
    (quotient, remainder)
  }

  def reduceCoeffs: (RealPolynomial, RealPolynomial) = {
    val x = GCDUtil.multigcd(this.coefficients.filter(x => x != 0).map(_.abs.value.toLong)).getOrElse(1L).toInt
    val divisor = RealPolynomial(x)
    ((this/divisor)._1, divisor)
  }

  override def toString(): String =
    if(this.coefficients == Seq(ring.zero)) "0"
    else {
      var toReturn = Array.empty[String]
      coefficients.reverse.indices.foreach{ i =>
        val coeffStr = {
          val tempStr = coefficients(i) match {
            case c if c == ring.zero || ((c == ring.one || c == ring.inverse(ring.one))&& i != 0) => ""
            case c => s"${c.abs}"
          }
          val tempStr2 = removeTrailingZeroes(removeLeadingZeroes(tempStr))
          if(tempStr.nonEmpty && tempStr2.last == '.') tempStr2.replace(".", "") else tempStr2
        }
        val expStr = i match {
          case exp if exp == 0 || coefficients(i) == 0 => ""
          case exp if exp == 1 => "x"
          case exp => s"x^$exp"
        }
        val multiStr = if(i > 0 && coeffStr.nonEmpty) "*" else ""
        val finalStr = s"$coeffStr$multiStr$expStr"
        if(finalStr.nonEmpty){
          if(coefficients(i) >= 0) {
            if(i == coefficients.indices.last) toReturn = toReturn ++ Array(finalStr)
            else toReturn = toReturn ++ Array("+" + finalStr)
          }else {
            if (i == coefficients.indices.last) toReturn = toReturn ++ Array("-" + finalStr)
            else toReturn = toReturn ++ Array("-" + finalStr)
          }
        }
      }
      toReturn.reverse.mkString("")
    }
}

object RealPolynomial{
  def apply(coefficients: Double*) = new RealPolynomial(coefficients.map(LargeNumber(_)):_*)
  def create(coefficients: LargeNumber*): RealPolynomial = new RealPolynomial(coefficients:_*)
  def zero: RealPolynomial = RealPolynomial(0.0)
  def one: RealPolynomial = RealPolynomial(1.0)
  def x: RealPolynomial = RealPolynomial(0.0, 1.0)
  def parse(polyString: String): RealPolynomial = {
    try {
      val tokens = polyString.replace(" ", "").replace("-", "+-").split("[+]").filter(_ != "")
      val coeffsAndExp: Seq[(Double, Int)] = tokens.map { token =>
        if (token.contains("x")) {
          val subTokens = token.split("x")
          if (subTokens(0).isEmpty)
            (1.0, subTokens(1).replace("^", "").toInt)
          else if (subTokens.length == 1)
            (subTokens(0).toDouble, 1)
          else
            (subTokens(0).toDouble, subTokens(1).replace("^", "").toInt)
        }
        else {
          (token.toDouble, 0)
        }
      }.toList
      val maxExp = coeffsAndExp.map(_._2).max
      val coeffs = (0 to maxExp).map { exp =>coeffsAndExp.filter(_._2 == exp).map(_._1).sum}.map(LargeNumber(_))
      RealPolynomial.create(coeffs: _*)
    }
    catch{
      case _: Exception => throw new IllegalArgumentException(s"input string $polyString is invalid")
    }
  }
}
