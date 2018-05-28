package org.bu.metcs789.algebraicStructures.fields

import org.bu.metcs789.algebraicStructures.rings.Ring

trait Field[T] extends Ring[T] {
  def div(a: T, b: T): T
}
