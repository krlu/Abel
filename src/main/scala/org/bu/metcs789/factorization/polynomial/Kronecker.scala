package org.bu.metcs789.factorization.polynomial

import org.apache.commons.math3.linear.{Array2DRowRealMatrix, ArrayRealVector, LUDecomposition}
import org.bu.metcs789.algebraicStructures.rings.polynomials.RealPolynomial
import org.bu.metcs789.combinationList
import org.bu.metcs789.factorization.GetAllFactors

import scala.collection.immutable

object Kronecker extends PolynomialFactorizationAlgo {
  override def apply(v1: RealPolynomial): Seq[RealPolynomial] = kroneckerFactorization(v1)
  /**
    * Factors a polynomial into a set of irreducible polynomials whose product equals this polynomial
    * Starts by checking possible roots from 0 to (deg(P)/2) then performs a more exhaustive search
    * If this polynomial is irreducible, this function returns a singleton set containing the input polynomial polynomial
    * @param p - Polynomial to be Factored
    * @return Seq[Polynomial] - Seq of factors of polynomial (if found)
    */
  private def kroneckerFactorization(p: RealPolynomial): Seq[RealPolynomial] = {
    if(p.degree <= 1) return Seq(p)
    val range = 0 to p.degree/2
    val roots = range.map(i => (i, p(i).toLong))
      .filter{case (_, pi) => pi == 0}
      .map{case(i,_) => i}
    var (quotient, remainder, factor) = (RealPolynomial.zero, RealPolynomial.one, RealPolynomial.one)

    if(roots.nonEmpty){
      factor = RealPolynomial(-roots.head, 1)
      val (q,r) = p/factor
      quotient = q
      remainder = r
    }
    else {
      val factorSets: immutable.List[List[Long]] = range.map(i => p(i).toLong).map(GetAllFactors(_).toList).toList
      var combos: Seq[List[Long]] = combinationList(factorSets)
      while ((remainder != RealPolynomial.zero || factor == RealPolynomial(-1) || factor == RealPolynomial.one) && combos.nonEmpty) {
        val x: Seq[Double] = combos.head.map(_.toDouble)
        combos = combos.filter(_ != x)
        factor = RealPolynomial(x.head, x(1))
        val (q, r) = p / factor
        quotient = q
        remainder = r
      }
    }
    if(remainder == RealPolynomial.zero && factor != RealPolynomial(-1.0) && factor != RealPolynomial.one)
      kroneckerFactorization(RealPolynomial(factor.coefficients:_*)) ++ kroneckerFactorization(RealPolynomial(quotient.coefficients:_*))
    else Seq(p)
  }

  /**
    * Helper function for Kronecker's factorization method that generates a potential factor
    * Uses Apache Common's solver to solve for coefficients, typically with Gaussian Elimination
    * @param range - range of possible degree of the factor polynomial (forms the matrix on the left hand side)
    * @param x - constant vector (on the right hand side of the equation)
    * @return Polynomial that might be a viable factor
    */
  @deprecated
  protected def generatePotentialFactor(range: Range, x: Seq[Double]): RealPolynomial = {
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
    RealPolynomial(solution: _*)
  }

}
