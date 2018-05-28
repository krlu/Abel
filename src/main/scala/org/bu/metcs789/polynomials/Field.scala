package org.bu.metcs789.polynomials

abstract class Field[T]{
  val zero: T
  val one: T
  def eq(a: T, b: T): Boolean
  def add(a: T, b: T): T
  def sub(a: T, b: T): T
  def mult(a: T, b: T): T
  def div(a: T, b: T): T
  def pow(a: T, exp: Int): T = if(exp == 0) one else mult(a, pow(a, exp - 1))
}

class Real extends Field[Double]{
  lazy val zero = 0.0
  lazy val one = 1.0
  override def add(a: Double, b: Double): Double = a + b
  override def sub(a: Double, b: Double): Double = a - b
  override def mult(a: Double, b: Double): Double = a * b
  override def div(a: Double, b: Double): Double = a/b
  override def eq(a: Double, b: Double): Boolean = a == b
}
object Real{ def apply(): Real = new Real() }
