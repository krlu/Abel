package org.bu.metcs789.Basics

object ModInverse extends ((Long, Long) => Long) {
  override def apply(value: Long, modulus: Long): Long = {
    require(modulus > 0 && value >= 0)
    (ExtendedGCD(value, modulus)._1 % modulus + modulus) % modulus
  }

  def main(args: Array[String]): Unit = {
    val phi = Totient(15688)
    println(FastExpWithMod(15688)(3, phi -1 ))
  }
}
