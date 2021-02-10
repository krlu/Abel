package org.bu.abel.algops

trait HasOrdering[T] {
  def compare(t1: T, t2: T): Int
}
