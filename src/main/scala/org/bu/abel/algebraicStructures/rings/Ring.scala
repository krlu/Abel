package org.bu.abel.algebraicStructures.rings

import org.bu.abel.algebraicStructures.Group

trait Ring[T] extends Group[T]{
  val one: T
  def mult(a: T, b: T): T
  def pow(a: T, exp: Int): T =
    if(exp == 0) one
    else if(exp == 1) a
    else if(exp % 2 == 0) pow(mult(a,a), exp/2)
    else mult(a, pow(mult(a,a),(exp-1)/2))
}
