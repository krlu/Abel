package org.bu.abel.algops.fields

import org.bu.abel.algops.rings.Ring

trait Field[T] extends Ring[T] {
  def div(a: T, b: T): T
  def remainder(a: T, b: T): T
}
