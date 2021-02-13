package org.bu.abel.types

import org.bu.abel.algops.fields.Complex

/**
  * Class representing a C number, denoted by C - symbol for set of C numbers
  *
  * @param re - real part
  * @param im - imaginary coefficient
  */
class C(val re: LargeNumber, val im: LargeNumber) {

  lazy val conjugate: C = C(re, -im)
  lazy val abs: LargeNumber = LargeNumber.sqrt(re * re + im * im)

  val conjugateProd: LargeNumber = re*re + im*im
  def +(other: C): C = C(re + other.re, im + other.im)
  def -(other: C): C = C(re - other.re, im - other.im)
  def *(x: LargeNumber):  C = C(re * x, im * x)
  def *(x: Double):  C = C(re * x, im * x)
  def *(other: C): C = C(re * other.re - im * other.im, re * other.im + im * other.re)
  def /(x: LargeNumber):  C = C(re / x, im / x)
  def /(x: Double):  C = C(re / x, im / x)
  def / (other: C): C = {
    val denominator = conjugateProd
    val numerator = this * other.conjugate
    numerator/denominator
  }
  def ^(e: Int): C = Complex().pow(this, e)
  def ^(c: C): C = C.exp((C.log(this) * c))
  def unary_- : C = C(-re, - im)

  override def equals(other: Any): Boolean = other match {
    case c: C => this.re == c.re && this.im == c.im
    case _ => false
  }
  def != (other: C): Boolean = !this.equals(other)
  def == (other: C): Boolean = this.equals(other)
  override def toString: String = s"$re + ${im}i"
}

object C{
  def log(c: C): C = C(LargeNumber.ln(c.abs), LargeNumber.atan(c.im/c.re))
  def exp(c: C): C = {
    val expreal = LargeNumber.exp(c.re)
    C(expreal * LargeNumber.cos(c.im), expreal * LargeNumber.sin(c.im))
  }
  def apply(re: LargeNumber, im: LargeNumber): C = new C(re, im)
  def apply(re: Double, im: Double): C = C(LargeNumber(re), LargeNumber(im))
  def apply(re: Int, im: Int): C = C(LargeNumber(re), LargeNumber(im))
  def zero: C = C(0,0)
  def one: C = C(1,0)
  def i: C = C(0,1)
}
