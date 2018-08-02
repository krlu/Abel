package org.bu.metcs789.algebraicStructures.fields

import org.bu.metcs789.algebraicStructures.types.Q

class Rational extends Field[Q]{
  override val zero: Q = Q(0,1)
  override val one: Q = Q(1,1)
  override def mult(a: Q, b: Q): Q = a * b
  override def div(a: Q, b: Q): Q = a/b
  override def add(a: Q, b: Q): Q = a + b
  override def sub(a: Q, b: Q): Q = a - b
  override def eq(a: Q, b: Q): Boolean = a == b
  override def inverse(a: Q): Q = a * -1
}
object Rational{ def apply(): Rational = new Rational() }
