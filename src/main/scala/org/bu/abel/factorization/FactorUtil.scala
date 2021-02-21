package org.bu.abel.factorization

import cc.redberry.rings.poly.PolynomialMethods
import org.bu.abel.algops.fields.Real
import org.bu.abel.algops.rings.{OrderedPolynomialRing, Ring}
import org.bu.abel.basics.PrimeUtil
import org.bu.abel.factorization.Integer.PollardRho
import org.bu.abel.factorization.polynomial.PolyFactorUtil.{toAbelPoly, toRingsPoly}
import org.bu.abel.types.LargeNumber
import org.bu.abel.types.polynomials.{OrderedPolynomial, RealPolynomial}

object FactorUtil{

  def getAllFactors(n: Long): Seq[Long] = {
    val primes = FactorUtil.primeFactorization(Math.abs(n))
    primes.indices.flatMap { i => primes.combinations(i).map(_.product) } ++ Seq(Math.abs(n))
  }
  def getAllFactors(rp: RealPolynomial): Seq[RealPolynomial] = {
    val primes: Seq[OrderedPolynomial[LargeNumber, Real]] = primeFactorization(rp).map(_.asInstanceOf[OrderedPolynomial[LargeNumber, Real]])
    val x: OrderedPolynomialRing[LargeNumber, Real] = OrderedPolynomialRing(Real())
    factorCombinations(primes, rp, x).map(p => RealPolynomial.create(p.coefficients:_*))
  }

  private def factorCombinations[T,U <: Ring[T]](primes: Seq[T], p: T, ring: U): Seq[T] =
    primes.indices.flatMap { i => primes.combinations(i).map{comb =>
      val prod = comb.foldLeft(ring.one)((a,b) => ring.mult(a,b))
      prod
    }} ++ Seq(p)

  def primeFactorization(p: RealPolynomial): Seq[RealPolynomial] = {
    val ringsP = toRingsPoly(p)
    val x = PolynomialMethods.Factor(ringsP)
    (0 until x.size()).map(x.get).map(toAbelPoly)
  }
  def primeFactorization(n: Long): Seq[Long] = if(n == 1) Seq(n) else factorizationHelper(n)

  private def factorizationHelper(n: Long): Seq[Long] = {
    if(PrimeUtil.isPrime(n)) return Seq(n)
    val f1 = PollardRho(n)
    if(f1 == 1) Seq(n) else factorizationHelper(f1) ++ factorizationHelper(n/f1)
  }
}
