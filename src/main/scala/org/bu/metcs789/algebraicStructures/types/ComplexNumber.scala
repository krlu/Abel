package org.bu.metcs789.algebraicStructures.types

import scala.math._

case class ComplexNumber(re: Double, im: Double) {
  val conjugate: ComplexNumber = ComplexNumber(re, -im)
  val conjugateProd: Double = re*re + im*im
  def +(other: ComplexNumber): ComplexNumber = ComplexNumber(re + other.re, im + other.im)
  def -(other: ComplexNumber): ComplexNumber = ComplexNumber(re - other.re, im - other.im)
  def *(x: Double):  ComplexNumber = ComplexNumber(re * x, im * x)
  def *(other: ComplexNumber): ComplexNumber = ComplexNumber(re * other.re - im * other.im, re * other.im + im * other.re)
  def /(x: Double):  ComplexNumber = ComplexNumber(re / x, im / x)
  def / (other: ComplexNumber): ComplexNumber = {
    val denominator = conjugateProd
    val numerator = this * other.conjugate
    numerator/denominator
  }

  def exp(c: ComplexNumber) : ComplexNumber = {
    val r = cosh(c.re) + sinh(c.re)
    ComplexNumber(cos(c.im), sin(c.im)) * r
  }

  def != (other: ComplexNumber): Boolean = !this.equals(other)
  def == (other: ComplexNumber): Boolean = this.equals(other)
  override def toString: String = s"$re + ${im}i"
}

object ComplexNumber{
  def apply(re: Double, im: Double): ComplexNumber = new ComplexNumber(re, im)
}
