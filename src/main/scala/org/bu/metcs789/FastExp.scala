package org.bu.metcs789

object FastExp extends ((Double, Int) => Double){
  override def apply(base: Double, exp: Int): Double = expBySquaring(1, base, exp)

  private def expBySquaring(currentVal: Double, base: Double, exp: Int): Double = exp match {
    case 0 => currentVal
    case 1 => currentVal*base
    case y if y < 0 => expBySquaring(currentVal, 1/base, -exp)
    case y if y%2 == 0 => expBySquaring(currentVal, base * base, exp/2)
    case _ => expBySquaring(currentVal*base, base*base, (exp-1)/2)
  }
}
