package org.bu.abel.types

import org.bu.abel.basics.GCD

/**
  * Class representing a rational number, denoted by Q - symbol for set of rationals
  * @param n - denominator for rational number
  * @param d - numerator for rational number
  */
case class Q(private val n: Int, private val d: Int) {

  private val gcd = GCD(n, d)._1.toInt
  val numerator: Int = n/gcd
  val denominator: Int = d/gcd

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
  def ^(e: Int) : Q = Q(Math.pow(numerator, e).toInt, Math.pow(denominator, e).toInt)

  def != (other: Q): Boolean = !this.equals(other)
  def == (other: Q): Boolean = this.equals(other)
  override def equals(other: Any): Boolean = other match {
    case q: Q => this.numerator == q.numerator && this.denominator == q.denominator
    case _ => false
  }
  override def toString: String = s"$numerator/$denominator"
}

object Q{
  def apply(numer: Int, denom: Int): Q = new Q(numer, denom)
}
