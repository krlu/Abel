package org.bu.abel.algebraicStructures.fields
import org.bu.abel.types.C

class Complex extends Field[C] {
  override val zero: C = C(0, 0)
  override val one: C = C(1, 0)

  override def inverse(a: C): C = a * -1
  override def add(a: C, b: C): C = a + b
  override def sub(a: C, b: C): C = a - b
  override def mult(a: C, b: C): C = a * b
  override def div(a: C, b: C): C = a/b
  override def eq(a: C, b: C): Boolean = a == b
}
