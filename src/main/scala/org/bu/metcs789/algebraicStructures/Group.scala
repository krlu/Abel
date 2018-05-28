package org.bu.metcs789.algebraicStructures

trait Group[T]{
  val zero: T
  def add(a: T, b: T): T
  def sub(a: T, b: T): T
  def eq(a: T, b: T): Boolean
}
