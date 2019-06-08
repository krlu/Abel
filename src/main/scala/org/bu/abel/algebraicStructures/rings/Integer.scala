package org.bu.abel.algebraicStructures.rings

class Integer extends Ring[Int]{
  override lazy val zero = 0
  override lazy val one = 1
  override def add(a: Int, b: Int): Int = a + b
  override def sub(a: Int, b: Int): Int = a - b
  override def mult(a: Int, b: Int): Int = a * b
  override def eq(a: Int, b: Int): Boolean = a == b
  override def inverse(a: Int): Int = -a
}
object Integer{ def apply(): Integer = new Integer() }
