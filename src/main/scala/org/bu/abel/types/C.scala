package org.bu.abel.types

import org.bu.abel.algebraicStructures.fields.Complex
import org.bu.abel.basics.LargeNumber

/**
  * Class representing a C number, denoted by C - symbol for set of C numbers
  *
  * @param re - real part
  * @param im - imaginary coefficient
  */
class C(val re: LargeNumber, val im: LargeNumber) {

  lazy val conjugate: C = C(re, -im)
  lazy val abs: LargeNumber = LargeNumber.sqrt(re * re + im * im)
  lazy val log: C = C(LargeNumber.ln(abs), LargeNumber.atan(im/re))
  lazy val exp: C = {
    val expreal = LargeNumber.exp(re)
    C(expreal * LargeNumber.cos(im), expreal * LargeNumber.sin(im))
  }

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

  def ^(c: C): C = (this * C.i * (Math.PI/2)).exp

  override def equals(other: Any): Boolean = other match {
    case c: C => this.re == c.re && this.im == c.im
    case _ => false
  }
  def != (other: C): Boolean = !this.equals(other)
  def == (other: C): Boolean = this.equals(other)
  override def toString: String = s"$re + ${im}i"
}

object C{
  def apply(re: LargeNumber, im: LargeNumber): C = new C(re, im)
  def apply(re: Int, im: Int): C = C(LargeNumber(re), LargeNumber(im))
  def zero: C = C(0,0)
  def one: C = C(1,0)
  def i: C = C(0,1)
}
