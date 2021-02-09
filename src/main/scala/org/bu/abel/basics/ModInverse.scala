package org.bu.abel.basics

object ModInverse extends ((Long, Long) => Long) {
  override def apply(value: Long, modulus: Long): Long = {
    require(modulus > 0 && value >= 0)
    (GCDUtil.extendedgcd(value, modulus)._1 % modulus + modulus) % modulus
  }
}
