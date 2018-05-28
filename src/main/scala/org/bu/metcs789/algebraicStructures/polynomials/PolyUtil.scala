package org.bu.metcs789.algebraicStructures.polynomials

import org.apache.commons.math3.linear.{Array2DRowRealMatrix, ArrayRealVector, LUDecomposition}
import org.bu.metcs789._
import org.bu.metcs789.factorization.GetAllFactors

import scala.collection.immutable

object PolyUtil {
  /**
    * Factors a polynomial into a set of irreducible polynomials whose product equals this polynomial
    * If this polynomial is irreducible, this function returns a singleton set containing the input polynomial polynomial
    * @param p - Polynomial to be Factored
    * @return Seq[Polynomial]
    */
  def kroneckerFactorization(p: RealPoly): Seq[RealPoly] = {
    if(p.degree <= 1) return Seq(p)
    val range = 0 to p.degree/2
    val roots = range.map(i => (i, p(i).toLong)).filter{case (_, pi) => pi == 0}.map{case(i,_) => i}
    val factorSets: immutable.List[List[Long]] = range
      .map(i => p(i).toLong)
      .map(GetAllFactors(_).toList).toList

    var (quotient, remainder, factor) = (Poly.zero, Poly.one, Poly.one)

    if(roots.nonEmpty){
      factor = Polynomial(-roots.head, 1)
      val (q,r) = p/factor
      quotient = q
      remainder = r
    }
    else {
      var combos: Seq[List[Long]] = combinationList(factorSets)
      while ((remainder != Poly.zero || factor == Poly(-1) || factor == Poly.one) && combos.nonEmpty) {
        val x: Seq[Double] = combos.head.map(_.toDouble)
        combos = combos.filter(_ != x)
        factor = generatePotentialFactor(range, x)
        val (q, r) = p / factor
        quotient = q
        remainder = r
      }
    }
    if(remainder == Poly.zero && factor != Polynomial(-1.0) && factor != Poly.one)
      kroneckerFactorization(Polynomial(factor.coefficients:_*)) ++ kroneckerFactorization(Polynomial(quotient.coefficients:_*))
    else Seq(p)
  }

  /**
    * Helper function for Kronecker's factorization method
    * Uses Apache Common's solver to solve for coefficients, typically with Gaussian Elimination
    * @param range - range of possible degree of the factor polynomial (forms the matrix on the left hand side)
    * @param x - constant vector (on the right hand side of the equation)
    * @return Polynomial that might be a viable factor
    */
  private def generatePotentialFactor(range: Range, x: Seq[Double]): Polynomial = {
    // entries of left hand side matrix
    val coeffs = range.map { i =>
      range.map { j =>
        Math.pow(i, j)
      }.toArray
    }.toArray
    val coefficients = new Array2DRowRealMatrix(coeffs, false)
    val solver = new LUDecomposition(coefficients).getSolver

    // right hand side vector
    val constants = new ArrayRealVector(Array[Double](x: _*), false)

    // solve system of equation to find coefficients for potential factor
    val solution = solver.solve(constants).toArray.toSeq
    Polynomial(solution: _*)
  }

  //  def berlekampFactorization(p: Polynomial): Set[Polynomial] = ???

  def GCD(p1: RealPoly, p2: RealPoly): RealPoly = if(p2 == Poly.zero) p1 else GCD(p2, p1 % p2)
}
