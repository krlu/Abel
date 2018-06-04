package org.bu.metcs789.algebraicStructures.polynomials

import org.bu.metcs789.algebraicStructures.fields.Real
import org.bu.metcs789.algebraicStructures.rings.Ring

/**
  * Generalization of a finite Polynomial
  * @param coeffs - coefficients for polynomial
  * @param ring - Algebraic Ring that governs set T of values
  * @tparam T - Type bound must support Ring structure
  * @tparam U - Type bound must extend Ring[T]
  */
class Poly[T, U <: Ring[T]](coeffs: T*)(implicit ring: U) extends (T => T){

  lazy val coefficients: Seq[T] = if(coeffs.isEmpty || coeffs.forall(ring.eq(_, ring.zero))) Seq(ring.zero) else coeffs.reverse.dropWhile(ring.eq(_, ring.zero)).reverse
  lazy val degree: Int = Math.max(0, coefficients.size - 1)
  val leadingCoeff: T = coefficients.head

  def + (other: Poly[T, U]): Poly[T, U] = Poly[T, U](this.coefficients.zipAll(other.coefficients, ring.zero, ring.zero).map{case(a,b) => ring.add(a,b)}:_*)(ring)
  def - (other: Poly[T, U]): Poly[T, U] = Poly[T, U](this.coefficients.zipAll(other.coefficients, ring.zero, ring.zero).map{case(a,b) => ring.sub(a,b)}:_*)(ring)
  def pow (exp: Int): Poly[T, U] = if(exp == 0) Poly[T, U](ring.one)(ring) else this * (this pow (exp-1))
  def * (other: Poly[T, U]): Poly[T, U] = {
    if(this.coefficients.isEmpty || other.coefficients.isEmpty) return Poly[T, U](ring.zero)(ring)
    coefficients.indices.map{ i =>
      val newCoeffs = (0 until i).map(_ => ring.zero) ++ other.coefficients.map(c => ring.mult(c,coefficients(i)))
      Poly[T, U](newCoeffs:_*)(ring)
    }.reduce((p1, p2) => p1 + p2)
  }

  def == (other: Poly[T, U]): Boolean = this.equals(other)
  def != (other: Poly[T, U]): Boolean = !this.equals(other)

  override def apply(v1: T): T = coefficients.indices.map{ i => ring.mult(coefficients(i),ring.pow(v1, i))}.reduce((a, b) => ring.add(a, b))
  override def equals(obj: scala.Any): Boolean = obj match {
    case other: Poly[T,U] =>
      if(this.coefficients.size != other.coefficients.size) false
      else (this.coefficients zip other.coefficients) forall { case (a, b) => a == b }
    case _ => false
  }

  def compose(other: T => T): Poly[T, U] = {
    other match {
      case poly: Poly[T, U] =>
        poly.coefficients.indices.map { i =>
          val c = poly.coefficients(i)
          Poly[T,U](c)(ring) * (this pow i)
        }.reduce((p1, p2) => p1 + p2)
      case _ => throw new IllegalArgumentException("input is not a polynomial!!")
    }
  }
}

object Poly{
  def apply[T, U <: Ring[T]](coeffs: T*)(field: U): Poly[T,U] = new Poly[T, U](coeffs:_*)(field)
  def apply(coeffs: Double*) = new Poly[Double, Real](coeffs:_*)(Real())
}

