package org.bu.abel.algops.rings


class IntegerRing extends Ring[Long]{
  override lazy val zero = 0
  override lazy val one = 1
  override def add(a: Long, b: Long): Long = a + b
  override def sub(a: Long, b: Long): Long = a - b
  override def mult(a: Long, b: Long): Long = a * b
  override def eq(a: Long, b: Long): Boolean = a == b
  override def inverse(a: Long): Long = -a
}
object IntegerRing{ def apply(): IntegerRing = new IntegerRing() }
