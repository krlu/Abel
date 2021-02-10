package org.bu.abel.algops.rings

import org.bu.abel.algops.HasOrdering
import org.bu.abel.algops.fields.Field
import org.bu.abel.types.polynomials.Polynomial

class PolynomialRing[T, U <: (Field[T] with HasOrdering[T])](val ring: U) extends Ring[Polynomial[T, U]] {

  override val one: Polynomial[T, U] = Polynomial[T,U](ring.one)(ring)
  override val zero: Polynomial[T, U] = Polynomial[T,U](ring.zero)(ring)

  override def inverse(a: Polynomial[T, U]): Polynomial[T, U] = a scale ring.inverse(ring.one)

  override def mult(a: Polynomial[T, U], b: Polynomial[T, U]): Polynomial[T, U] = a mult b

  override def add(a: Polynomial[T, U], b: Polynomial[T, U]): Polynomial[T, U] = a add b

  override def sub(a: Polynomial[T, U], b: Polynomial[T, U]): Polynomial[T, U] = a sub b

  override def eq(a: Polynomial[T, U], b: Polynomial[T, U]): Boolean = a == b
}

object PolynomialRing{
  def apply[T, U <: (Field[T] with HasOrdering[T])](ring: U): PolynomialRing[T, U] = new PolynomialRing(ring)
}