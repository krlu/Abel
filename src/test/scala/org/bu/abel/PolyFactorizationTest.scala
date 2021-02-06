package org.bu.abel

import org.bu.abel.types.polynomials.{PolyUtil, RealPolynomial}
import org.bu.abel.factorization.polynomial.{Kronecker, NewtonsMethod, SquareFreeFactorization}
import org.scalatest.{FlatSpec, Matchers}

class PolyFactorizationTest extends FlatSpec with Matchers{

  "Kronecker's Method" should "Factor Polynomials" in {
    val f4 = RealPolynomial(1,1,1)
    val f5 = RealPolynomial(1,1,1)
    val p4 = f4 * f5
    assert(Kronecker(p4).toSet == Set(f4, f5))
    val p1 = RealPolynomial(-1, 0, 0, 0, 1)
    val p2 = RealPolynomial(-1, 0, 1)
    val p3 = RealPolynomial(-16, -24, -4, 10, 6, 1)
    val f1 = RealPolynomial(2,1)
    val f2 = RealPolynomial(-2,0,1)

    val factors1 = Set(RealPolynomial(1, 1), RealPolynomial(-1, 1), RealPolynomial(1, 0, 1))
    val factors2 = Set(RealPolynomial(1, 1), RealPolynomial(-1, 1))

    assert(Kronecker(p1).toSet == factors1)
    val x = factors1.reduce((a, b) => a * b)
    assert(x == p1)
    assert(Kronecker(p2).toSet == factors2)
    assert(factors2.reduce((a, b) => a * b) == p2)
    assert(Kronecker(RealPolynomial(1, 0, 1)) == Seq(RealPolynomial(1, 0, 1)))
    assert(Kronecker(p3) == List(f1, f1, f1, f2))

    for (i <- 1 to 5) {
      val coeffs = (1 to i).map(_ => (Math.random()*5).toInt + 1)
      val realFactors = coeffs.map(coeff => RealPolynomial(coeff, 1))
      val p = realFactors.foldLeft(RealPolynomial.one){(p1 ,p2) => p1 * p2}
      val factors = Kronecker(p)
      assert(factors.size == i)
      assert(factors.sortWith(comparePolys) == realFactors.sortWith(comparePolys))
    }
    for (i <- -10 to 10) {
      val pi = RealPolynomial(i)
      assert(Kronecker(pi) == Seq(pi))
    }
  }

  "Newton's factorization" should "Factor polynomials via newton's method for root finding" in {
    val p1 = RealPolynomial(-1, 0, 0, 0, 1)
    val p2 = RealPolynomial(-1, 0, 1)

    val factors1 = Set(RealPolynomial(1, 1), RealPolynomial(-1, 1), RealPolynomial(1, 0, 1))
    val factors2 = Set(RealPolynomial(1, 1), RealPolynomial(-1, 1))
    assert(NewtonsMethod()(p1).toSet == factors1)
    val x = factors1.reduce((a, b) => a * b)

    assert(x == p1)
    assert(NewtonsMethod()(p2).toSet == factors2)
    assert(factors2.reduce((a, b) => a * b) == p2)
    assert(NewtonsMethod()(RealPolynomial(1, 0, 1)) == Seq(RealPolynomial(1, 0, 1)))

    for (i <- 1 to 10) {
      val coeffs = (1 to i).toList.map(_ => (Math.random()*5).toInt + 1)
      val realFactors = coeffs.map(coeff => RealPolynomial(coeff, 1))
      val p = realFactors.foldLeft(RealPolynomial.one){(p1 ,p2) => p1 * p2}
      try {
        val factors = NewtonsMethod()(p)
        assert(factors.size == i)
        assert(factors.sortWith(comparePolys) == realFactors.sortWith(comparePolys))
      } catch {
        case e: IllegalStateException =>
          assert(e.getMessage == "found saddle point")
      }
    }
    for (i <- -10 to 10) {
      val pi = RealPolynomial(i)
      assert(NewtonsMethod()(pi) == Seq(pi))
    }
  }

  "Square free factorization" should "Factor polynomials" in {
    val p1 = RealPolynomial(1,1)
    for(i <- 1 to 20) {
      val p = p1 ^ i
      val sqFreeFactors = SquareFreeFactorization(p)
      assert(sqFreeFactors == List.fill(i)(p1))
    }
  }

  "Rings factorization" should "Factor polynomials quickly and correctly up to degree 18" in {
    val totalFactors = 8
    val factors = (1 to totalFactors).map(i => RealPolynomial(7, i, i+1))
    var product = RealPolynomial.one
    for(i <- 0 until totalFactors){
      product = product * factors(i)
      assert(PolyUtil.factorRealPolynomial(product) == factors.take(i+1).toSet)
    }
  }
  private def comparePolys(p1: RealPolynomial, p2: RealPolynomial): Boolean =
    p1.coefficients.head > p2.coefficients.head
}
