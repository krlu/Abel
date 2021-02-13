package org.bu.abel.algops.rings

import org.bu.abel.algops.fields.Field
import org.bu.abel.basics.GCDUtil
import org.bu.abel.types.polynomials.Polynomial

class IntegerModN(modulus: Long) extends Ring[Long]{

  override val one: Long = 1
  override val zero: Long = 0

  override def mult(a: Long, b: Long): Long = mod(mod(a) * mod(b))
  override def inverse(a: Long): Long = mod(-a)
  override def add(a: Long, b: Long): Long = mod(mod(a) + mod(b))
  override def sub(a: Long, b: Long): Long = mod(mod(a) - mod(b))
  override def eq(a: Long, b: Long): Boolean = mod(a) == mod(b)

  def mod(value: Long): Long = {
    if(value < 0)
      (value%modulus + modulus)%modulus
    else value%modulus
  }


}

object IntegerModN{
  def modInverse(value: Long, modulus: Long): Long = {
    require(modulus > 0 && value >= 0)
    (GCDUtil.extendedgcd(value, modulus)._1 % modulus + modulus) % modulus
  }
  def apply(modulus: Long): IntegerModN = new IntegerModN(modulus)
}


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
}
