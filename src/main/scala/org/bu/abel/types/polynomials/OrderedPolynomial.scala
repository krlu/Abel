package org.bu.abel.types.polynomials

import org.bu.abel.algops.HasOrdering
import org.bu.abel.algops.fields.Field

/**
  * Generalization of a finite Polynomial
  * Supports addition, subtraction, multiplication, exponentiation
  * @param coeffs - coefficients for polynomial
  * @param field - Algebraic Ring that governs set T of values
  * @tparam T - Type bound must support Ring structure
  * @tparam U - Type bound must extend Ring[T]
  */
class OrderedPolynomial[T, U <: Field[T] with HasOrdering[T]](coeffs: T*)(implicit override val field: U) extends Polynomial[T,U](coeffs:_*){

  def div(other: OrderedPolynomial[T, U]): (OrderedPolynomial[T, U], OrderedPolynomial[T, U]) = {
    val zeroPoly = OrderedPolynomial(field.zero)(field)
    require(other != zeroPoly)
    var quotient = zeroPoly
    var remainder = OrderedPolynomial(coeffs:_*)(field)
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
      val factor = (OrderedPolynomial(field.zero, field.one)(field) pow (remainder.degree - divisionIndex - other.degree)) mult
        OrderedPolynomial(field.div(field.sub(rLeadCoeff,field.remainder(rLeadCoeff,otherLeadCoeff)),otherLeadCoeff))(field)
      if(factor == zeroPoly)
        return (quotient, remainder)
      remainder = OrderedPolynomial((remainder sub (factor mult other)).coefficients:_*)(field)
      quotient = OrderedPolynomial((quotient add factor).coefficients:_*)(field)
    }
    (quotient, remainder)
  }

  private def abs(t: T): T = {
    if(field.compare(t, field.zero) < 0) field.inverse(t) else t
  }
}

protected[abel] object OrderedPolynomial{
  def apply[T, U <: Field[T] with HasOrdering[T]](coeffs: T*)(field: U): OrderedPolynomial[T,U] = new OrderedPolynomial[T, U](coeffs:_*)(field)
}