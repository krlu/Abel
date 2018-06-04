package org.bu.metcs789.algebraicStructures.fields

class Real extends Field[Double]{
  lazy val zero = 0.0
  lazy val one = 1.0
  override def add(a: Double, b: Double): Double = a + b
  override def sub(a: Double, b: Double): Double = a - b
  override def mult(a: Double, b: Double): Double = a * b
  override def div(a: Double, b: Double): Double = a/b
  override def eq(a: Double, b: Double): Boolean = a == b
  override def inverse(a: Double): Double = -a
}
object Real{ def apply(): Real = new Real() }
