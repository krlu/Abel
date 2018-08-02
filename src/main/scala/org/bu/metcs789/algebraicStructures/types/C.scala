package org.bu.metcs789.algebraicStructures.types

import scala.math._

case class C(re: Double, im: Double) {
  val conjugate: C = C(re, -im)
  val conjugateProd: Double = re*re + im*im
  def +(other: C): C = C(re + other.re, im + other.im)
  def -(other: C): C = C(re - other.re, im - other.im)
  def *(x: Double):  C = C(re * x, im * x)
  def *(other: C): C = C(re * other.re - im * other.im, re * other.im + im * other.re)
  def /(x: Double):  C = C(re / x, im / x)
  def / (other: C): C = {
    val denominator = conjugateProd
    val numerator = this * other.conjugate
    numerator/denominator
  }
  def exp(c: C) : C = {
    val r = cosh(c.re) + sinh(c.re)
    C(cos(c.im), sin(c.im)) * r
  }

  def != (other: C): Boolean = !this.equals(other)
  def == (other: C): Boolean = this.equals(other)
  override def toString: String = s"$re + ${im}i"
}

object C{
  def apply(re: Double, im: Double): C = new C(re, im)
}
