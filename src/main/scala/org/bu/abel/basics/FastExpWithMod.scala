package org.bu.abel.basics

class FastExpWithMod(modulus: Long) extends ((Long, Long) => Long) {
  override def apply(base: Long, exp: Long): Long = {
    var x = 1
    for (_ <- 0 until exp.toInt)
      x = (x * base.toInt) % modulus.toInt
    x
  }
}

object FastExpWithMod{
  def apply(modulus: Long): FastExpWithMod = new FastExpWithMod(modulus)
}

