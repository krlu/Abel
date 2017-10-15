package org.bu.metcs789.Basics

object FastExp extends ((Double, Long) => Double){
  override def apply(base: Double, exp: Long): Double = expBySquaring(1, base, exp)

  private def expBySquaring(currentVal: Double, base: Double, exp: Long): Double = exp match {
    case 0 => currentVal
    case 1 => currentVal*base
    case y if y < 0 => expBySquaring(currentVal, 1/base, -exp)
    case y if y%2 == 0 => expBySquaring(currentVal, base * base, exp/2)
    case _ => expBySquaring(currentVal*base, base, exp - 1)
  }
}

protected class FastExpWithMod(modulus: Long)extends ((Double, Long) => Double){
  override def apply(base: Double, exp: Long): Double = {
    var x = 1
    for(i <- 0 until exp.toInt)
      x = (x * base.toInt) % modulus.toInt
    x
  }
}

object FastExpWithMod{
  def apply(modulus: Long): FastExpWithMod = new FastExpWithMod(modulus)
}
