package org.bu.metcs789.polynomials

import org.apache.commons.math3.linear.{Array2DRowRealMatrix, LUDecomposition, ArrayRealVector}
import org.bu.metcs789._
import org.bu.metcs789.factorization.PrimeFactorization
import scala.collection.immutable

object PolyUtil {

  def kroneckerFactorization(p: Polynomial): Seq[Polynomial] = {
    if(p == Polynomial.zero || p == Polynomial.one || p == Polynomial(-1.0)) return Seq(p)
    val range = 0 to p.degree/2
    val factorSets: immutable.List[List[Long]] = range
      .map(i => p(i).toLong)
      .map(PrimeFactorization(_).toSet.toList).toList
    var (quotient, remainder) = (Polynomial.zero, Polynomial.one)
    var factor = Polynomial.one
    var combos = combinationList(factorSets)
    while((remainder != Polynomial.zero || factor == Polynomial(-1)) && combos.nonEmpty){
      // left hand side vector
      val coeffs = range.map{ i => range.map { j =>
        Math.pow(i, j)
      }.toArray
      }.toArray
      val coefficients = new Array2DRowRealMatrix(coeffs, false)
      val solver = new LUDecomposition(coefficients).getSolver

      // right hand side vector
      val x = choose(combos.iterator).map(_.toDouble)
      combos = combos.filter(_ != x)
      val constants = new ArrayRealVector(Array[Double](x:_*), false)

      // solve system of equation to find coefficients for potential factor
      val solution = solver.solve(constants).toArray.toSeq
      factor = Polynomial(solution:_*)
      val (q, r) = p/factor
      quotient = q
      remainder = r
    }
    if(remainder == Polynomial.zero && factor != Polynomial(-1.0) && factor != Polynomial.one)
      kroneckerFactorization(factor) ++ kroneckerFactorization(quotient)
    else Seq(p)
  }

//  def berlekampFactorization(p: Polynomial): Set[Polynomial] = ???

  def GCD(p1: Polynomial, p2: Polynomial): Polynomial = {
    if(p2 == Polynomial.zero) p1
    else {
      GCD(p2, p1 % p2)
    }
  }
}

object foo{
  def main(args: Array[String]): Unit = {
    println(Math.pow(0, 0))
  }
}
