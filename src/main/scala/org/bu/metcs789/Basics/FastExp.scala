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

  def main(args: Array[String]) {
    val (mod, target) = (13, 12)
    for(i <- 0 until mod){
      val result = FastExpWithMod(mod)(i, 2)
      if(result == target)
        println(result, i)
    }
    println(ExtendedGCD(13,36))
    println(FastExpWithMod(1039)(892,260))
//    val mod = 13
//    val exp = 2
//    val x = ModInverse(exp,12)
//    println(x)
//    val y = FastExpWithMod(mod)(12,x.toInt)
//    println(y)
//    val result = FastExp(y, exp) % mod
//    println(result)
  }
}
