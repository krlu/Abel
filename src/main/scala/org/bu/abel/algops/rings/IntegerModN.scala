package org.bu.abel.algops.rings

import org.bu.abel.basics.ModInverse

class IntegerModN(modulus: Long) extends Ring[Long]{

  override val one: Long = 1
  override val zero: Long = 0

  override def mult(a: Long, b: Long): Long = mod(mod(a) * mod(b))
  override def inverse(a: Long): Long = ModInverse(a, modulus)
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
  def apply(modulus: Long): IntegerModN = new IntegerModN(modulus)
}