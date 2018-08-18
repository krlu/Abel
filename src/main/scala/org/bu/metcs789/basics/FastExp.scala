package org.bu.metcs789.basics

object FastExp extends ((Long, Long) => Long){
  override def apply(base: Long, exp: Long): Long = expBySquaring(1, base, exp)
  private def expBySquaring(currentVal: Long, base: Long, exp: Long): Long = exp match {
    case 0 => currentVal
    case 1 => currentVal*base
    case y if y < 0 =>
      expBySquaring(currentVal, 1/base, -exp)
    case y if y%2 == 0 => expBySquaring(currentVal, base * base, exp/2)
    case _ => expBySquaring(currentVal*base, base, exp - 1)
  }
}

protected class FastExpWithMod(modulus: Long)extends ((Long, Long) => Long) {
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

