package org.bu.abel.algops.fields

import org.bu.abel.algops.HasOrdering
import org.bu.abel.types.Q

class Rational extends Field[Q] with HasOrdering[Q]{
  override val zero: Q = Q(0,1)
  override val one: Q = Q(1,1)
  override def mult(a: Q, b: Q): Q = a * b
  override def div(a: Q, b: Q): Q = a/b
  override def add(a: Q, b: Q): Q = a + b
  override def sub(a: Q, b: Q): Q = a - b
  override def eq(a: Q, b: Q): Boolean = a == b
  override def inverse(a: Q): Q = a * -1
  override def remainder(a: Q, b: Q): Q = Q(0,0)

  override def compare(t1: Q, t2: Q): Int = {
    val bothDenoms = t1.denominator * t2.denominator
    (t1.numerator * bothDenoms).compareTo(t2.numerator * bothDenoms)
  }
}
object Rational{ def apply(): Rational = new Rational() }
