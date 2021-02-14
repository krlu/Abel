package org.bu.abel.algops.fields

import org.bu.abel.algops.rings.Ring

trait Field[T] extends Ring[T] {

  def div(a: T, b: T): T = mult(a,multInv(b))
  def remainder(a: T, b: T): T
  def multInv(a: T): T
  override def pow(a: T, exp: Long): T = expBySquaring(one, a, exp)

  @scala.annotation.tailrec
  private def expBySquaring(currentVal: T, base: T, exp: Long): T = exp match {
    case y if y < 0 => expBySquaring(currentVal, this.div(this.one, base), -y)
    case 0 => currentVal
    case 1 => mult(currentVal, base)
    case y if y%2 == 0 => expBySquaring(currentVal, mult(base, base), exp/2)
    case _ => expBySquaring(mult(currentVal,base), base, exp - 1)
  }
}
