package org.bu.abel.algebraicStructures.fields

import org.bu.abel.algebraicStructures.rings.Ring

trait Field[T] extends Ring[T] {
  def div(a: T, b: T): T
}
