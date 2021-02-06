package org.bu.abel.algebraicStructures.rings

import org.bu.abel.types.polynomials.Polynomial

class PolynomialRing[U, T <: Ring[U]](val ring: T) extends Ring[Polynomial[U, T]] {

  override val one: Polynomial[U, T] = Polynomial(ring.one)(ring)
  override val zero: Polynomial[U, T] = Polynomial(ring.zero)(ring)

  override def inverse(a: Polynomial[U, T]): Polynomial[U, T] = a scale ring.inverse(ring.one)

  override def mult(a: Polynomial[U, T], b: Polynomial[U, T]): Polynomial[U, T] = a mult b

  override def add(a: Polynomial[U, T], b: Polynomial[U, T]): Polynomial[U, T] = a add b

  override def sub(a: Polynomial[U, T], b: Polynomial[U, T]): Polynomial[U, T] = a sub b

  override def eq(a: Polynomial[U, T], b: Polynomial[U, T]): Boolean = a == b
}
