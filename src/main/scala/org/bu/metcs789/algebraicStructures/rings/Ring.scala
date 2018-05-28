package org.bu.metcs789.algebraicStructures.rings

import org.bu.metcs789.algebraicStructures.Group

trait Ring[T] extends Group[T]{
  val one: T
  def mult(a: T, b: T): T
  def pow(a: T, exp: Int): T = if(exp == 0) one else mult(a, pow(a, exp - 1))
}
