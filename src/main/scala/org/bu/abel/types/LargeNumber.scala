package org.bu.abel.types

import ch.obermuhlner.math.big.BigFloat
import org.apache.commons.math3.fraction.BigFraction

/**
  * Scala Wrapper for Big Floats, enables natural arithmetic expressions
  * @param value - BigFloat from BigMath library
  */
// TODO: needs unit testing
// TODO: Underlying BigMath library is slow, may want to use a different library
class LargeNumber(val value: BigFloat) {
  def + (other: LargeNumber): LargeNumber = LargeNumber(this.value.add(other.value))
  def - (other: LargeNumber): LargeNumber = LargeNumber(this.value.subtract(other.value))
  def * (other: LargeNumber): LargeNumber = LargeNumber(this.value.multiply(other.value))
  def / (other: LargeNumber): LargeNumber = {
    val quotient = LargeNumber(this.value.divide(other.value))
    if(LargeNumber.abs(quotient) < LargeNumber(0.0000000000000000000000000001))
      LargeNumber(0.0)
    else quotient
  }
  def ^ (other: LargeNumber): LargeNumber = LargeNumber(this.value.pow(other.value))
  def < (other: LargeNumber): Boolean = this.value.isLessThan(other.value)
  def > (other: LargeNumber): Boolean = this.value.isGreaterThan(other.value)
  def <= (other: LargeNumber): Boolean = this.value.isLessThanOrEqual(other.value)
  def >= (other: LargeNumber): Boolean = this.value.isGreaterThanOrEqual(other.value)
  def == (other: LargeNumber): Boolean = this.value.isEqual(other.value)
  def != (other: LargeNumber): Boolean = !this.value.isEqual(other.value)
  def % (other: LargeNumber): LargeNumber = LargeNumber(this.value.remainder(other.value))

  def + (other: Double): LargeNumber = LargeNumber(this.value.add(other))
  def - (other: Double): LargeNumber = LargeNumber(this.value.subtract(other))
  def * (other: Double): LargeNumber = LargeNumber(this.value.multiply(other))
  def / (other: Double): LargeNumber = LargeNumber(this.value.divide(other))
  def ^ (other: Double): LargeNumber = LargeNumber(this.value.pow(other))
  def < (other: Double): Boolean = this < LargeNumber(other)
  def > (other: Double): Boolean = this > LargeNumber(other)
  def <= (other: Double): Boolean = this <= LargeNumber(other)
  def >= (other: Double): Boolean = this >= LargeNumber(other)
  def == (other: Double): Boolean = this == LargeNumber(other)
  def != (other: Double): Boolean = this != LargeNumber(other)
  def % (other: Double): LargeNumber = this % LargeNumber(other)

  def abs: LargeNumber = LargeNumber.abs(this)

  def + (other: Int): LargeNumber = LargeNumber(this.value.add(other))
  def - (other: Int): LargeNumber = LargeNumber(this.value.subtract(other))
  def * (other: Int): LargeNumber = LargeNumber(this.value.multiply(other))
  def / (other: Int): LargeNumber = LargeNumber(this.value.divide(other))
  def ^ (other: Int): LargeNumber = LargeNumber(this.value.pow(other))
  def < (other: Int): Boolean = this < LargeNumber(other)

  def isInteger: Boolean = {
    val splitStr = this.value.toString.split("\\.").toList
    if(splitStr.size == 1) true
    else LargeNumber.fromString(splitStr(1)) == 0
  }
  def unary_- : LargeNumber = LargeNumber(BigFloat.negate(this.value))

  override def equals(obj: Any): Boolean = {
    obj match {
      case num: LargeNumber => this.value.isEqual(num.value)
      case _ => false
    }
  }

  override def toString: String = value.toString
}

object LargeNumber{
  def main(args: Array[String]): Unit = {
    var a = LargeNumber(1234)
    for(_ <- 1 to 10){
      a = a^(-1)
      val frac = new BigFraction(a.value.toDouble, 0.00000002D, 10000)
      println(a.value.toDouble, frac)
    }
  }

  /**
    * Rounds to nearest whole number
    * @param value - in large number
    * @return Large Number as an integer
    */
  def round(value: LargeNumber): LargeNumber = {
    val splitStr = value.value.toString.split("\\.").toList
    if(splitStr.size == 1) value
    else if(LargeNumber.fromString(s"0.${splitStr(1)}") < 0.5)
      LargeNumber.fromString(splitStr.head)
    else {
      val integerTerm = LargeNumber.fromString(splitStr.head)
      if(integerTerm < 0) integerTerm - 1
      else integerTerm + 1
    }
  }

  def roundDown(value: LargeNumber): LargeNumber = {
    val splitStr = value.value.toString.split("\\.").toList
    if(splitStr.size == 1) value
    else LargeNumber.fromString(splitStr.head)
  }
  def abs(value: LargeNumber): LargeNumber = LargeNumber(BigFloat.abs(value.value))
  def exp(value: LargeNumber): LargeNumber = LargeNumber(BigFloat.exp(value.value))
  def ln(value: LargeNumber): LargeNumber = LargeNumber(BigFloat.log(value.value))
  def cos(value: LargeNumber): LargeNumber = LargeNumber(BigFloat.cos(value.value))
  def sin(value: LargeNumber): LargeNumber = LargeNumber(BigFloat.sin(value.value))
  def tan(value: LargeNumber): LargeNumber = LargeNumber(BigFloat.tan(value.value))
  def atan(value: LargeNumber): LargeNumber = {
    if(value.value == BigFloat.POSITIVE_INFINITY)
      LargeNumber(Math.atan(Double.PositiveInfinity))
    else if(value.value == BigFloat.POSITIVE_INFINITY)
      LargeNumber(Math.atan(Double.NegativeInfinity))
    else
      LargeNumber(BigFloat.atan(value.value))
  }
  def sqrt(value: LargeNumber): LargeNumber = LargeNumber(BigFloat.sqrt(value.value))
  def apply(value: BigFloat): LargeNumber = new LargeNumber(value)
  def fromString(value: String, precision: Int = 100): LargeNumber = new LargeNumber(BigFloat.context(precision).valueOf(value))
  def apply(value: Double, precision: Int = 100): LargeNumber = new LargeNumber(BigFloat.context(precision).valueOf(value))
}