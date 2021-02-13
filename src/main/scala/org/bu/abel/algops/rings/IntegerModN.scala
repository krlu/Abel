package org.bu.abel.algops.rings

import org.bu.abel.algops.HasOrdering
import org.bu.abel.algops.fields.Field
import org.bu.abel.basics.GCDUtil
import org.bu.abel.types.polynomials.Polynomial

class IntegerModN(modulus: Long) extends Field[Long] with HasOrdering[Long]{

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

  override def compare(t1: Long, t2: Long): Int = t1.compareTo(t2)
  override def remainder(a: Long, b: Long): Long = mod(a%b)
  override def multInv(a: Long): Long = IntegerModN.modInverse(a, modulus)
}

object IntegerModN{
  def modInverse(value: Long, modulus: Long): Long = {
    require(modulus > 0 && value >= 0)
    (GCDUtil.extendedgcd(value, modulus)._1 % modulus + modulus) % modulus
  }
  def apply(modulus: Long): IntegerModN = new IntegerModN(modulus)
}