package org.bu.metcs789.polynomials

import org.apache.commons.math3.linear.{Array2DRowRealMatrix, LUDecomposition, ArrayRealVector}
import org.bu.metcs789._
import org.bu.metcs789.factorization.PrimeFactorization
import scala.collection.immutable

object PolyUtil {
  def kroneckerFactorization(p: Polynomial): Set[Polynomial] = {
    if(p == Polynomial.zero || p == Polynomial.one || p == Polynomial(-1)) return Set(p)
    val range = 0 to p.degree/2
    val factorSets: immutable.List[List[Long]] = range
      .map(i => p(i).toLong)
      .map(PrimeFactorization(_).toSet.toList).toList
    var (quotient, remainder) = (Polynomial.zero, Polynomial.one)
    var f = Polynomial.one
    var combos = combinationList(factorSets)
    while(remainder != Polynomial.zero && combos.nonEmpty){
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
      val solution = solver.solve(constants).toArray.toSeq
      f = Polynomial(solution:_*)
      val (q, r) = p/f
      quotient = q
      remainder = r
    }
    println(quotient, f)
    Thread.sleep(1000)
    if(remainder == Polynomial.zero) kroneckerFactorization(f) ++ kroneckerFactorization(quotient) else Set(p)
  }

  def berlekampFactorization(p: Polynomial): Set[Polynomial] = ???

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
