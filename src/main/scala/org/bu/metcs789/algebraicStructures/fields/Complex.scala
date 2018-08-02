package org.bu.metcs789.algebraicStructures.fields
import org.bu.metcs789.algebraicStructures.types.ComplexNumber

class Complex extends Field[ComplexNumber] {
  override val zero: ComplexNumber = ComplexNumber(0, 0)
  override val one: ComplexNumber = ComplexNumber(1, 0)

  override def inverse(a: ComplexNumber): ComplexNumber = a * -1
  override def add(a: ComplexNumber, b: ComplexNumber): ComplexNumber = a + b
  override def sub(a: ComplexNumber, b: ComplexNumber): ComplexNumber = a - b
  override def mult(a: ComplexNumber, b: ComplexNumber): ComplexNumber = a * b
  override def div(a: ComplexNumber, b: ComplexNumber): ComplexNumber = a/b
  override def eq(a: ComplexNumber, b: ComplexNumber): Boolean = a == b
}
