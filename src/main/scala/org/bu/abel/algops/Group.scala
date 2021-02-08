package org.bu.abel.algops

trait Group[T]{
  val zero: T
  def inverse(a: T): T
  def add(a: T, b: T): T
  def sub(a: T, b: T): T
  def eq(a: T, b: T): Boolean
}
