package org.bu.abel.basics

import ch.obermuhlner.math.big.BigFloat

/**
  * Scala Wrapper for Big Floats, enables natural arithmetic expressions
  * @param value - BigFloat from BigMath library
  */
class LargeNumber(val value: BigFloat) {
  def + (other: LargeNumber): LargeNumber = LargeNumber(this.value.add(other.value))
  def - (other: LargeNumber): LargeNumber = LargeNumber(this.value.subtract(other.value))
  def * (other: LargeNumber): LargeNumber = LargeNumber(this.value.multiply(other.value))
  def / (other: LargeNumber): LargeNumber = LargeNumber(this.value.divide(other.value))
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

// TODO: LargeNumber needs unit testing!!!
object LargeNumber{
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
  def fromString(value: String, precision: Int = 100 ): LargeNumber = new LargeNumber(BigFloat.context(precision).valueOf(value))
  def apply(value: Double, precision: Int = 100): LargeNumber = new LargeNumber(BigFloat.context(precision).valueOf(value))
}