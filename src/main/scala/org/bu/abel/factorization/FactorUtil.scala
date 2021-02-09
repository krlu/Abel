package org.bu.abel.factorization

import org.bu.abel.algops.fields.Real
import org.bu.abel.algops.rings.{IntegerRing, PolynomialRing, Ring}
import org.bu.abel.basics.PrimeUtil
import org.bu.abel.factorization.Integer.{PollardRho, PrimeFactorization}
import org.bu.abel.factorization.polynomial.PolyFactorUtil
import org.bu.abel.types.LargeNumber
import org.bu.abel.types.polynomials.{Polynomial, RealPolynomial}

object FactorUtil{

  def getAllFactors(n: Long): Seq[Long] = {
    val primes = FactorUtil.primeFactorization(Math.abs(n))
    primes.indices.flatMap { i => primes.combinations(i).map(_.product) } ++ Seq(Math.abs(n))
  }
  def getAllFactors(rp: RealPolynomial): Seq[RealPolynomial] = {
    val primes: Seq[Polynomial[LargeNumber, Real]] = PolyFactorUtil.factorRealPolynomial(rp).toSeq.map(_.asInstanceOf[Polynomial[LargeNumber, Real]])
    val x: PolynomialRing[LargeNumber, Real] = new PolynomialRing(Real())
    factorCombinations(primes, rp, x).map(p => RealPolynomial.create(p.coefficients:_*))
  }

  private def factorCombinations[T,U <: Ring[T]](primes: Seq[T], p: T, ring: U): Seq[T] =
    primes.indices.flatMap { i => primes.combinations(i).map{comb =>
      val prod = comb.foldLeft(ring.one)((a,b) => ring.mult(a,b))
      println(comb, prod, "........")
      prod
    }} ++ Seq(p)

  def primeFactorization(p: RealPolynomial): Seq[RealPolynomial] = PolyFactorUtil.factorRealPolynomial(p).toSeq
  def primeFactorization(n: Long): Seq[Long] = if(n == 1) Seq(n) else factorizationHelper(n)

  private def factorizationHelper(n: Long): Seq[Long] = {
    if(PrimeUtil.isPrime(n)) return Seq(n)
    val f1 = PollardRho(n)
    if(f1 == 1) Seq(n) else factorizationHelper(f1) ++ factorizationHelper(n/f1)
  }
}
