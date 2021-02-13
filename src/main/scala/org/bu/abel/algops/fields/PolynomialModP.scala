package org.bu.abel.algops.fields

import org.bu.abel.types.polynomials.Polynomial

class PolynomialModP[T,U <: Field[T]](modulus: Polynomial[T,U], field: U) extends Field[Polynomial[T,U]]{

  override val one: Polynomial[T, U] = Polynomial[T,U](field.one)(field)
  override val zero: Polynomial[T, U] = Polynomial[T,U](field.zero)(field)

  override def mult(a: Polynomial[T,U], b: Polynomial[T,U]): Polynomial[T,U] = mod(mod(a) mult mod(b))
  override def inverse(a: Polynomial[T,U]): Polynomial[T,U] = mod(a.invert)
  override def add(a: Polynomial[T,U], b: Polynomial[T,U]): Polynomial[T,U] = mod(mod(a) add mod(b))
  override def sub(a: Polynomial[T,U], b: Polynomial[T,U]): Polynomial[T,U] = mod(mod(a) sub mod(b))
  override def eq(a: Polynomial[T,U], b: Polynomial[T,U]): Boolean = mod(a) == mod(b)

  def mod(value: Polynomial[T,U]): Polynomial[T,U] = ???

  override def div(a: Polynomial[T, U], b: Polynomial[T, U]): Polynomial[T, U] = ???

  override def remainder(a: Polynomial[T, U], b: Polynomial[T, U]): Polynomial[T, U] = ???

  override def multInv(a: Polynomial[T, U]): Polynomial[T, U] = ???
}
