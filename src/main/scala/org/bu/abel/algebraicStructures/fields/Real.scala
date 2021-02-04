package org.bu.abel.algebraicStructures.fields

class Real extends Field[BigDecimal]{
  lazy val zero = 0.0
  lazy val one = 1.0
  override def add(a: BigDecimal, b: BigDecimal): BigDecimal = a + b
  override def sub(a: BigDecimal, b: BigDecimal): BigDecimal = a - b
  override def mult(a: BigDecimal, b: BigDecimal): BigDecimal = a * b
  override def div(a: BigDecimal, b: BigDecimal): BigDecimal = a/b
  override def eq(a: BigDecimal, b: BigDecimal): Boolean = a == b
  override def inverse(a: BigDecimal): BigDecimal = -a
}
object Real{ def apply(): Real = new Real() }
