package org.bu.abel.algebraicStructures.fields

import org.bu.abel.basics.LargeNumber

class Real extends Field[LargeNumber]{
  lazy val zero: LargeNumber = LargeNumber(0.0)
  lazy val one: LargeNumber = LargeNumber(1.0)
  override def add(a: LargeNumber, b: LargeNumber): LargeNumber = a + b
  override def sub(a: LargeNumber, b: LargeNumber): LargeNumber = a - b
  override def mult(a: LargeNumber, b: LargeNumber): LargeNumber = a * b
  override def div(a: LargeNumber, b: LargeNumber): LargeNumber = a/b
  override def eq(a: LargeNumber, b: LargeNumber): Boolean = a == b
  override def inverse(a: LargeNumber): LargeNumber = -a
}
object Real{ def apply(): Real = new Real() }
