package org.bu.abel.algops.fields

import org.bu.abel.algops.HasOrdering
import org.bu.abel.types.LargeNumber

class Real extends Field[LargeNumber] with HasOrdering[LargeNumber]{
  lazy val zero: LargeNumber = LargeNumber(0.0)
  lazy val one: LargeNumber = LargeNumber(1.0)
  override def add(a: LargeNumber, b: LargeNumber): LargeNumber = a + b
  override def sub(a: LargeNumber, b: LargeNumber): LargeNumber = a - b
  override def mult(a: LargeNumber, b: LargeNumber): LargeNumber = a * b
  override def div(a: LargeNumber, b: LargeNumber): LargeNumber = a/b
  override def eq(a: LargeNumber, b: LargeNumber): Boolean = a == b
  override def inverse(a: LargeNumber): LargeNumber = -a
  override def remainder(a: LargeNumber, b: LargeNumber): LargeNumber = a%b
  override def compare(t1: LargeNumber, t2: LargeNumber): Int = t1.value.compareTo(t2.value)
  override def multInv(a: LargeNumber): LargeNumber = LargeNumber(1)/a
}
object Real{ def apply(): Real = new Real() }
