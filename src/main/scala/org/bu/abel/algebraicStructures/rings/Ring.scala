package org.bu.abel.algebraicStructures.rings

import org.bu.abel.algebraicStructures.Group

trait Ring[T] extends Group[T]{
  val one: T
  def mult(a: T, b: T): T
  def pow(a: T, exp: Long): T = expBySquaring(one, a, exp)
  private def expBySquaring(currentVal: T, base: T, exp: Long): T = exp match {
    case 0 => currentVal
    case 1 => mult(currentVal, base)
    case y if y%2 == 0 => expBySquaring(currentVal, mult(base, base), exp/2)
    case _ => expBySquaring(mult(currentVal,base), base, exp - 1)
  }
}
