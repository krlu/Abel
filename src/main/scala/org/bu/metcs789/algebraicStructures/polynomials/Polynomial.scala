package org.bu.metcs789.algebraicStructures.polynomials

import org.bu.metcs789.algebraicStructures.rings.Ring

/**
  * Generalization of a finite Polynomial
  * @param coeffs - coefficients for polynomial
  * @param ring - Algebraic Ring that governs set T of values
  * @tparam T - Type bound must support Ring structure
  * @tparam U - Type bound must extend Ring[T]
  */
protected[polynomials] class Polynomial[T, U <: Ring[T]](coeffs: T*)(implicit val ring: U) extends (T => T){

  private type PolyType = Polynomial[T, U]

  lazy val coefficients: Seq[T] = if(coeffs.isEmpty || coeffs.forall(ring.eq(_, ring.zero))) Seq(ring.zero) else coeffs.reverse.dropWhile(ring.eq(_, ring.zero)).reverse
  lazy val degree: Int = Math.max(0, coefficients.size - 1)
  val leadingCoeff: T = coefficients.head

  def + (other: Polynomial[T, U]): Polynomial[T, U] = Polynomial[T, U](this.coefficients.zipAll(other.coefficients, ring.zero, ring.zero).map{case(a,b) => ring.add(a,b)}:_*)(ring)
  def - (other: Polynomial[T, U]): Polynomial[T, U] = Polynomial[T, U](this.coefficients.zipAll(other.coefficients, ring.zero, ring.zero).map{case(a,b) => ring.sub(a,b)}:_*)(ring)
  def pow (exp: Int): Polynomial[T, U] = if(exp == 0) Polynomial[T, U](ring.one)(ring) else this * (this pow (exp-1))
  def * (other: Polynomial[T, U]): Polynomial[T, U] = {
    if(this.coefficients.isEmpty || other.coefficients.isEmpty) return Polynomial[T, U](ring.zero)(ring)
    coefficients.indices.map{ i =>
      val newCoeffs = (0 until i).map(_ => ring.zero) ++ other.coefficients.map(c => ring.mult(c,coefficients(i)))
      Polynomial[T, U](newCoeffs:_*)(ring)
    }.reduce((p1, p2) => p1 + p2)
  }

  def == (other: Polynomial[T, U]): Boolean = this.equals(other)
  def != (other: Polynomial[T, U]): Boolean = !this.equals(other)

  override def apply(v1: T): T = coefficients.indices.map{ i => ring.mult(coefficients(i),ring.pow(v1, i))}.reduce((a, b) => ring.add(a, b))
  override def equals(obj: scala.Any): Boolean = obj match {
    case other: Polynomial[T,U] =>
      if(this.coefficients.size != other.coefficients.size) false
      else (this.coefficients zip other.coefficients) forall { case (a, b) => a == b }
    case _ => false
  }

  def compose(other: T => T): Polynomial[T, U] = {
    other match {
      case poly: Polynomial[T, U] =>
        poly.coefficients.indices.map { i =>
          val c = poly.coefficients(i)
          Polynomial[T,U](c)(ring) * (this pow i)
        }.reduce((p1, p2) => p1 + p2)
      case _ => throw new IllegalArgumentException("input is not a polynomial!!")
    }
  }

  override def toString(): String =
    if(this == RealPolynomial.zero) "0.0"
    else {
      coefficients.indices.map { i =>
        val coeffStr = coefficients(i) match {
          case c if c == ring.zero || (c == ring.one && i != ring.zero) => ""
          case c => s"($c)"
        }
        val expStr = i match {
          case exp if exp == 0 || coefficients(i) == 0 => ""
          case exp if exp == 1 => "x"
          case exp => s"x^$exp"
        }
        s"$coeffStr$expStr"
      }.filter(_.nonEmpty).reverse.mkString(" + ")
    }
}

protected object Polynomial{
  def apply[T, U <: Ring[T]](coeffs: T*)(field: U): Polynomial[T,U] = new Polynomial[T, U](coeffs:_*)(field)
}
