package org.bu.abel.algops.fields

import org.bu.abel.types.polynomials.{Polynomial, RealPolynomial}

class PolynomialModP[T,U <: Field[T]](modulus: RealPolynomial, field: Real) extends Field[RealPolynomial]{

  override val one: RealPolynomial = RealPolynomial.create(field.one)
  override val zero: RealPolynomial = RealPolynomial.create(field.zero)

  override def mult(a: RealPolynomial, b: RealPolynomial): RealPolynomial = mod(mod(a) * mod(b))
  override def inverse(a: RealPolynomial): RealPolynomial = mod(-a)
  override def add(a: RealPolynomial, b: RealPolynomial): RealPolynomial = mod(mod(a) + mod(b))
  override def sub(a: RealPolynomial, b: RealPolynomial): RealPolynomial = mod(mod(a) - mod(b))
  override def eq(a: RealPolynomial, b: RealPolynomial): Boolean = mod(a) == mod(b)

  def mod(value: RealPolynomial): RealPolynomial = remainder(value, modulus)

  override def remainder(a: RealPolynomial, b: RealPolynomial): RealPolynomial = a % b

  override def multInv(a: RealPolynomial): RealPolynomial = ???
}
