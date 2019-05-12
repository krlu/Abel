package org.bu.metcs789.algebraicStructures.rings.polynomials

import org.bu.metcs789.algebraicStructures.rings.Ring

/**
  * Generalization of a finite Polynomial
  * Supports addition, subtraction, multiplication, exponentiation
  * @param coeffs - coefficients for polynomial
  * @param ring - Algebraic Ring that governs set T of values
  * @tparam T - Type bound must support Ring structure
  * @tparam U - Type bound must extend Ring[T]
  */
protected class Polynomial[T, U <: Ring[T]](coeffs: T*)(implicit val ring: U) extends (T => T){

  private type PolyType = Polynomial[T, U]

  lazy val coefficients: Seq[T] =
    if(coeffs.isEmpty || coeffs.forall(ring.eq(_, ring.zero))) Seq(ring.zero)
    else coeffs.reverse.dropWhile(ring.eq(_, ring.zero)).reverse
  lazy val degree: Int = coefficients.size - 1
  val leadingCoeff: T = coefficients.reverse.head

  /**
    * Using this polynomial P, Computes P(x) for some x, where x belongs to the set T with structure Ring[T]
    * @param x - some input variable
    * @return p(x) - output value belonging to T with structure Ring[T]
    */
  override def apply(x: T): T = coefficients.indices.map{ i =>
    ring.mult(coefficients(i),ring.pow(x, i))
  }.reduce((a, b) => ring.add(a, b))

  /**
    * Addition operation
    * @param other - another polynomial
    * @return the sum of two polynomials
    */
  protected def add(other: Polynomial[T, U]): Polynomial[T, U] =
    Polynomial[T, U](this.coefficients.zipAll(other.coefficients, ring.zero, ring.zero)
      .map{case(a,b) => ring.add(a,b)}:_*)(ring)

  protected def add(scalar: T): Polynomial[T, U] = this add Polynomial[T, U](scalar)(ring)

  /**
    * Subtraction operation
    * @param other - another polynomial
    * @return the difference between two polynomials
    */
  protected def sub(other: Polynomial[T, U]): Polynomial[T, U] =
    Polynomial[T, U](this.coefficients.zipAll(other.coefficients, ring.zero, ring.zero)
      .map{case(a,b) => ring.sub(a,b)}:_*)(ring)

  protected def sub(scalar: T): Polynomial[T, U] = this sub Polynomial[T, U](scalar)(ring)

  /**
    * multiplication operation, computed using distribution property of rings
    * @param other - another polynomial
    * @return the product of two polynomials
    */
  protected def mult(other: Polynomial[T, U]): Polynomial[T, U] = {
    if(this.coefficients.isEmpty || other.coefficients.isEmpty) return Polynomial[T, U](ring.zero)(ring)
    coefficients.indices.map{ i =>
      val newCoeffs = (0 until i).map(_ => ring.zero) ++ other.coefficients.map(c => ring.mult(c,coefficients(i)))
      Polynomial[T, U](newCoeffs:_*)(ring)
    }.reduce((p1, p2) => p1 add p2)
  }

  /**
    * Multiplies all coefficients in polynomial by a scalar value under Ring[T]
    * @param scalar - lives with the set T
    * @return a new scaled Polynomial
    */
  protected def scale(scalar: T): Polynomial[T, U] = Polynomial(this.coeffs.map(c => ring.mult(c, scalar)):_*)(ring)

  /**
    * Exponentiation operation, computed by recursively multiplying two polynomials
    * @param exp - A non-negative integer
    * @return exponent of a polynomial two the power of some
    */
  protected def pow(exp: Int): Polynomial[T, U] = {
    require(exp >= 0)
    if(exp == 0) Polynomial[T, U](ring.one)(ring)
    else if(exp == 1) this
    else if(exp % 2 == 0) (this mult this) pow (exp/2)
    else this mult ((this mult this) pow ((exp - 1)/2))
  }

  def == (other: Polynomial[T, U]): Boolean = this.equals(other)
  def != (other: Polynomial[T, U]): Boolean = !this.equals(other)

  /**
    * Overrides equals method.
    * Computes two polynomials to be equal if and only if their coefficients are the same at each index
    * @param obj - other polynomial
    * @return true if and only if this polynomial is equal to the other
    */
  override def equals(obj: scala.Any): Boolean = obj match {
    case other: Polynomial[T,U] =>
      if(this.coefficients.size != other.coefficients.size) false
      else (this.coefficients zip other.coefficients) forall { case (a, b) => a == b }
    case _ => false
  }
  def canEqual(a: Any): Boolean = a.isInstanceOf[Polynomial[T, U]]

  override def hashCode: Int = coefficients.map(_.hashCode() % 100).mkString("").toInt

  def compose(other: T => T): Polynomial[T, U] = {
    other match {
      case poly: Polynomial[T, U] =>
        poly.coefficients.indices.map { i =>
          val c = poly.coefficients(i)
          Polynomial[T,U](c)(ring) mult (this pow i)
        }.reduce((p1, p2) => p1 add p2)
      case _ => throw new IllegalArgumentException("input is not a polynomial!!")
    }
  }

  override def toString(): String =
    if(this.coefficients == Seq(ring.zero)) "0.0"
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
