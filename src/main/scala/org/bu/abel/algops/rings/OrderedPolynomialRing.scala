package org.bu.abel.algops.rings

import org.bu.abel.algops.HasOrdering
import org.bu.abel.algops.fields.Field
import org.bu.abel.types.polynomials.OrderedPolynomial

class OrderedPolynomialRing[T, U <: (Field[T] with HasOrdering[T])](val field: U) extends Ring[OrderedPolynomial[T, U]] {

  override val one: OrderedPolynomial[T, U] = OrderedPolynomial[T,U](field.one)(field)
  override val zero: OrderedPolynomial[T, U] = OrderedPolynomial[T,U](field.zero)(field)

  override def inverse(a: OrderedPolynomial[T, U]): OrderedPolynomial[T, U] = OrderedPolynomial((a scale field.inverse(field.one)).coefficients:_*)(field)

  override def mult(a: OrderedPolynomial[T, U], b: OrderedPolynomial[T, U]): OrderedPolynomial[T, U] = OrderedPolynomial((a mult b).coefficients:_*)(field)

  override def add(a: OrderedPolynomial[T, U], b: OrderedPolynomial[T, U]): OrderedPolynomial[T, U] = OrderedPolynomial((a add b).coefficients:_*)(field)

  override def sub(a: OrderedPolynomial[T, U], b: OrderedPolynomial[T, U]): OrderedPolynomial[T, U] = OrderedPolynomial((a sub b).coefficients:_*)(field)

  override def eq(a: OrderedPolynomial[T, U], b: OrderedPolynomial[T, U]): Boolean = a == b
}

object OrderedPolynomialRing{
  def apply[T, U <: (Field[T] with HasOrdering[T])](ring: U): OrderedPolynomialRing[T, U] = new OrderedPolynomialRing(ring)
}