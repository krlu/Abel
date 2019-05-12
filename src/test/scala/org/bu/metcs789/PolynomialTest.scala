package org.bu.metcs789

import org.bu.metcs789.algebraicStructures.rings.polynomials.{PolyUtil, RealPolynomial}
import org.bu.metcs789.factorization.polynomial.{Kronecker, NewtonsMethod}
import org.scalatest.{FlatSpec, Matchers}

class PolynomialTest extends FlatSpec with Matchers{

  "A RealPolynomial" should "support addition" in {
    val p1 = RealPolynomial(1,1)
    val p2 = RealPolynomial(1,2,4)
    val p3 = p1 + p2
    assert(p3.coefficients == Seq(2,3,4))
    assert(p3 == RealPolynomial(2,3,4))
  }

  "A Real Polynomial" should "hash almost uniquely" in {
    val x1 = Set(RealPolynomial(3,1), RealPolynomial(5,1), RealPolynomial(1,1), RealPolynomial(4,1), RealPolynomial(7,1))
    val x2 = Set(RealPolynomial(3,1), RealPolynomial(5,1), RealPolynomial(1,1), RealPolynomial(4,1), RealPolynomial(7,1))
    assert(x1 == x2)
  }

  "A RealPolynomial" should "support subtraction" in {
    val p1 = RealPolynomial(1,1)
    val p2 = RealPolynomial(1,2,4)
    val p3 = p1 - p2
    assert(p3.coefficients == Seq(0,-1,-4))
  }

  "A RealPolynomial" should "support multiplication" in {
    val p1 = RealPolynomial(3,1)
    val p2 = RealPolynomial(9,-3,1)
    val p3 = p1 * p2
    val p4 = p1 * 2
    assert(p3.coefficients == Seq(27,0,0,1))
    assert(p4 == RealPolynomial(6,2))
  }

  "A RealPolynomial" should "support exponentiation" in {
    val p1 = RealPolynomial(1,1)
    val p0 = p1 ^ 0
    val p2 = p1 ^ 1
    val p3 = p1 ^ 3
    assert(p0 == RealPolynomial.one)
    assert(p2 == p1)
    assert(p2.coefficients == Seq(1,1))
    assert(p3.coefficients == Seq(1,3,3,1))
    for(i <- 0 to 10){
      val pi = p1 ^ i
      val px = pi * p1
      val coeffs = (0 to i).map{ a => i choose a}.map(_.toDouble)
      assert(pi == Array.fill(i)(p1).toList.foldLeft(RealPolynomial.one)((a, b) => a * b))
      assert(px == (p1 ^ (i + 1)))
      assert(pi.coefficients == coeffs)
    }
  }

  implicit class Combinations(n: Int) {
    private def fact(n: Int): Int = (1 to n).product
    def ! : Int = fact(n)
    def choose(k: Int): Int = fact(n) / (fact(n - k) * fact(k))
  }

  "A RealPolynomial" should "support differentiation and antiderivation" in {
    val p1 = RealPolynomial(1,2,1)
    val p2 = p1.derivative
    assert(p2.coefficients == Seq(2,2))
    assert(p2.antiDerivative.coefficients == Seq(0,2,1))
  }

  "A RealPolynomial" should "support division and mod operations" in {
    val p1 = RealPolynomial(-1,0,0,0,1)
    val p2 = RealPolynomial(1,1)
    val p3 = RealPolynomial(-16, -24, -4, 10, 6, 1)
    val p4 = RealPolynomial(4,4,1)
    val (quotient, remainder) = p1 / p2
    val modY = p1 % p2
    assert(quotient == RealPolynomial(-1,1,-1,1))
    assert(quotient*p2 == p1)
    assert(remainder == modY)
    assert(remainder == RealPolynomial.zero)
    assert(p3/p4 == (RealPolynomial(-4,-2,2,1), RealPolynomial.zero))
    assert(p4* (RealPolynomial(-6,4,5) + RealPolynomial(2,-6,-3,1)) == p3)
  }

  "A RealPolynomial" should "support toString" in {
    val p1 = RealPolynomial(1,-1)
    val p2 = p1 ^ 3
    val p3 = RealPolynomial(-1,1,-1,1)
    assert(p3.toString == "x^3 + (-1.0)x^2 + x + (-1.0)")
    assert(p2.toString == "(-1.0)x^3 + (3.0)x^2 + (-3.0)x + (1.0)")
  }

  "A RealPolynomial" should "compose with other polynomials" in {
    val p1 = RealPolynomial(1, 1)
    val p2 = RealPolynomial(0, 0, 1)
    val p3 = p1 compose p2
    assert(p3 == RealPolynomial(1,2,1))
    assert(p3(3) == 16)
  }

  "RealPolynomial Util" should "compute GCD between polynomials" in {
    val p1 = RealPolynomial(0,1) * RealPolynomial(1,1)
    val p2 = RealPolynomial(1,0,1) * RealPolynomial(1,1)
    assert(PolyUtil.GCD(p1, p2) == RealPolynomial(1,1))

    val p3 = RealPolynomial(-1,1) * RealPolynomial(1,1)
    val p4 = RealPolynomial(1,1) ^ 2
    assert(PolyUtil.GCD(p3, p4) == RealPolynomial(1,1))

    val p5 = RealPolynomial(-1,0,1)
    val p6 = RealPolynomial(2,2)
    assert(PolyUtil.GCD(p5, p6) == RealPolynomial(1,1))
  }

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

    for (i <- 1 to 15) {
      val coeffs = (1 to i).toList.map(_ => (Math.random()*5).toInt + 1)
      val realFactors = coeffs.map(coeff => RealPolynomial(coeff, 1))
      val p = realFactors.foldLeft(RealPolynomial.one){(p1 ,p2) => p1 * p2}
      val factors = NewtonsMethod()(p)
      assert(factors.size == i)
      assert(factors.sortWith(comparePolys) == realFactors.sortWith(comparePolys))
    }
    for (i <- -10 to 10) {
      val pi = RealPolynomial(i)
      assert(NewtonsMethod()(pi) == Seq(pi))
    }
  }
  private def comparePolys(p1: RealPolynomial, p2: RealPolynomial): Boolean =  p1.coefficients.head > p2.coefficients.head

}
