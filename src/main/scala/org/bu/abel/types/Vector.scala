package org.bu.abel.types

import org.bu.abel.algebraicStructures.fields.Field

class Vector[T, F <: Field[T]](val elements: T*)(implicit val field: F){

  val dimension: Int = this.elements.size

  private def combine(other: Vector[T, F], func: (T, T) => T): Vector[T,F] = {
    if(other.dimension == this.dimension) {
      new Vector[T, F]((this.elements zip other.elements).map{case (a, b) => func(a,b)}:_*)(field)
    } else
      throw new IllegalArgumentException(s"Dimension mismatch, v1 dim was ${this.dimension} and v2 dim was ${other.dimension}")
  }
  def +(other: Vector[T, F]): Vector[T, F] = combine(other, field.add)
  def -(other: Vector[T, F]): Vector[T, F] = combine(other, field.sub)
  def *(other: Vector[T,F]): T = combine(other, (a,b) => field.mult(a,b)).elements.foldLeft(field.zero)((a: T,b: T) => field.add(a,b))
  def scale(scale: T) = Vector(this.elements.map(e => field.mult(e, scale)))
  def unary_- = new Vector[T, F](this.elements.map(x => field.inverse(x)):_*)(field)
}
