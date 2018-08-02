package org.bu.metcs789.algebraicStructures.types

import org.bu.metcs789.basics.GCD

case class Q(numerator: Int, denominator: Int) {
  require(denominator != 0)
  def +(other: Q): Q = helper(this, other, (a,b) => a + b)
  def -(other: Q): Q = helper(this, other, (a,b) => a - b)
  private def helper(a: Q, b: Q, f: (Int, Int) => Int): Q = {
    f(a.numerator * b.denominator, b.numerator * a.denominator)
    val numer =  f(a.numerator * b.denominator, b.numerator * a.denominator)
    val denom = a.denominator * b.denominator
    val gcd = GCD(numer, denom)._1.toInt
    Q(numer/gcd, denom/gcd)
  }
  def *(other: Q): Q = {
    val numer = numerator * other.numerator
    val denom = denominator * other.denominator
    val gcd = GCD(numer, denom)._1.toInt
    Q(numer/gcd, denom/gcd)
  }
  def *(x: Int):  Q = Q(numerator * x, denominator)
  def /(x: Int):  Q = this * Q(1, x)
  def / (other: Q): Q = this * Q(other.denominator,other.numerator)
  def exp(e: Int) : Q = Q(Math.pow(numerator, e).toInt, Math.pow(denominator, e).toInt)
  def != (other: Q): Boolean = !this.equals(other)
  def == (other: Q): Boolean = this.equals(other)
  override def toString: String = s"$numerator/$denominator"
}

object Q{
  def apply(numer: Int, denom: Int): Q = new Q(numer, denom)
}