package org.bu.abel.types.polynomials

import org.bu.abel.algops.HasOrdering
import org.bu.abel.algops.fields.Field
import org.bu.abel.algops.rings.PolynomialRing

/**
  * Generalization of a finite Polynomial
  * Supports addition, subtraction, multiplication, exponentiation
  * @param coeffs - coefficients for polynomial
  * @param field - Algebraic Ring that governs set T of values
  * @tparam T - Type bound must support Ring structure
  * @tparam U - Type bound must extend Ring[T]
  */
class Polynomial[T, U <: Field[T] with HasOrdering[T]](coeffs: T*)(implicit val field: U) extends (T => T){

  private type PolyType = Polynomial[T, U]

  lazy val coefficients: Seq[T] =
    if(coeffs.isEmpty || coeffs.forall(field.eq(_, field.zero))) Seq(field.zero)
    else coeffs.reverse.dropWhile(field.eq(_, field.zero)).reverse
  lazy val degree: Int = coefficients.size - 1
  val leadingCoeff: T = coefficients.reverse.head

  /**
    * Using this polynomial P, Computes P(x) for some x, where x belongs to the set T with structure Ring[T]
    * @param x - some input variable
    * @return p(x) - output value belonging to T with structure Ring[T]
    */
  override def apply(x: T): T = coefficients.indices.map{ i =>
    field.mult(coefficients(i),field.pow(x, i))
  }.reduce((a, b) => field.add(a, b))

  protected[abel] def invert : Polynomial[T, U] = Polynomial[T,U](this.coefficients.map(field.inverse):_*)(field)

  /**
    * Addition operation
    * @param other - another polynomial
    * @return the sum of two polynomials
    */
  protected[abel] def add(other: Polynomial[T, U]): Polynomial[T, U] =
    Polynomial[T, U](this.coefficients.zipAll(other.coefficients, field.zero, field.zero)
      .map{case(a,b) => field.add(a,b)}:_*)(field)

  protected[abel]  def add(scalar: T): Polynomial[T, U] = this add Polynomial[T, U](scalar)(field)

  /**
    * Subtraction operation
    * @param other - another polynomial
    * @return the difference between two polynomials
    */
  protected[abel]  def sub(other: Polynomial[T, U]): Polynomial[T, U] =
    Polynomial[T, U](this.coefficients.zipAll(other.coefficients, field.zero, field.zero)
      .map{case(a,b) => field.sub(a,b)}:_*)(field)


  protected[abel]  def sub(scalar: T): Polynomial[T, U] = this sub Polynomial[T, U](scalar)(field)

  /**
    * multiplication operation, computed using distributive property of rings
    * @param other - another polynomial
    * @return the product of two polynomials
    */
  protected[abel] def mult(other: Polynomial[T, U]): Polynomial[T, U] = {
    if(this.coefficients.isEmpty || other.coefficients.isEmpty) return Polynomial[T, U](field.zero)(field)
    coefficients.indices.map{ i =>
      val newCoeffs = (0 until i).map(_ => field.zero) ++ other.coefficients.map(c => field.mult(c,coefficients(i)))
      Polynomial[T, U](newCoeffs:_*)(field)
    }.reduce((p1, p2) => p1 add p2)
  }

  protected[abel] def div (other: Polynomial[T, U]): (Polynomial[T, U], Polynomial[T, U]) = {
    val zeroPoly = Polynomial(field.zero)(field)
    require(other != zeroPoly)
    var quotient = zeroPoly
    var remainder = Polynomial(coeffs:_*)(field)
    if(this.coefficients.isEmpty) return (other, zeroPoly)
    while(remainder.degree >= other.degree) {
      var divisionIndex = 0
      var rLeadCoeff = remainder.coefficients.reverse(divisionIndex)
      val otherLeadCoeff = other.leadingCoeff
      // inner while loop to enforce integer division
      while(field.compare(abs(rLeadCoeff),abs(otherLeadCoeff)) < 0){
        divisionIndex += 1
        if(divisionIndex + other.degree > remainder.degree)
          return (quotient, remainder)
        rLeadCoeff = remainder.coefficients.reverse(divisionIndex)
      }
      val factor = (Polynomial(field.zero, field.one)(field) pow (remainder.degree - divisionIndex - other.degree)) mult
        Polynomial(field.div(field.sub(rLeadCoeff,field.remainder(rLeadCoeff,otherLeadCoeff)),otherLeadCoeff))(field)
      if(factor == zeroPoly)
        return (quotient, remainder)
      remainder = remainder sub (factor mult other)
      quotient = quotient add factor
    }
    (quotient, remainder)
  }

  def abs(a: T): T = if(field.compare(a, field.zero) < 0) field.inverse(a) else a

  /**
    * Multiplies all coefficients in polynomial by a scalar value under Ring[T]
    * @param scalar - lives with the set T
    * @return a new scaled Polynomial
    */
  protected[abel]  def scale(scalar: T): Polynomial[T, U] = Polynomial(this.coeffs.map(c => field.mult(c, scalar)):_*)(field)

  /**
    * Exponentiation operation, computed by recursively by squaring
    * @param exp - A non-negative integer
    * @return exponent of a polynomial two the power of some
    */
  protected[abel] def pow(exp: Long): Polynomial[T, U] = {
    require(exp >= 0)
    val pr = new PolynomialRing[T,U](field)
    pr.pow(this, exp)
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

  override def hashCode: Int = {
    val number = coefficients.map(_.toString).mkString("")
      .filterNot(c => List('-', '.', 'E').contains(c))
    BigDecimal(number).toInt
  }

  def compose(other: T => T): Polynomial[T, U] = {
    other match {
      case poly: Polynomial[T, U] =>
        poly.coefficients.indices.map { i =>
          val c = poly.coefficients(i)
          Polynomial[T,U](c)(field) mult (this pow i)
        }.reduce((p1, p2) => p1 add p2)
      case _ => throw new IllegalArgumentException("input is not a polynomial!!")
    }
  }

  override def toString(): String =
    if(this.coefficients == Seq(field.zero)) "0.0"
    else {
      coefficients.indices.map { i =>
        val coeffStr = coefficients(i) match {
          case c if c == field.zero || ((c == field.one || c == field.inverse(field.one)) && i != field.zero) => ""
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

protected[abel] object Polynomial{
  def apply[T, U <: Field[T] with HasOrdering[T]](coeffs: T*)(field: U): Polynomial[T,U] = new Polynomial[T, U](coeffs:_*)(field)
}