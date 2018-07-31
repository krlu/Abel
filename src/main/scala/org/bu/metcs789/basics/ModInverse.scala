package org.bu.metcs789.basics

object ModInverse extends ((Long, Long) => Long) {
  override def apply(value: Long, modulus: Long): Long = {
    require(modulus > 0 && value >= 0)
    (ExtendedGCD(value, modulus)._1 % modulus + modulus) % modulus
  }
}
