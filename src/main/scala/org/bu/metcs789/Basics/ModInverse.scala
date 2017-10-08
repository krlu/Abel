package org.bu.metcs789.Basics

object ModInverse extends ((Long, Long) => Long) {
  override def apply(value: Long, modulus: Long): Long = {
    require(modulus > 0 && value >= 0)
    val gcd = GCD(value, modulus)._1
    val totient = Totient(modulus)
    gcd match {
      case 1 => FastExp(value, totient - 1).toLong % modulus
      case _ => ExtendedGCD(value, modulus)._1 % modulus
    }
  }
}
