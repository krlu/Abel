package org.bu.abel

import org.bu.abel.types.polynomials.{PolyUtil, RealPolynomial}
import org.scalatest.{FlatSpec, Matchers}

class PolynomialTest extends FlatSpec with Matchers{

  implicit class Combinations(n: Int) {
    private def fact(n: Int): Int = (1 to n).product
    def ! : Int = fact(n)
    def choose(k: Int): Int = fact(n) / (fact(n - k) * fact(k))
  }
  private def randomIntegerPoly: RealPolynomial = {
    val numCoeffs = choose((1 to 3).iterator)
    val possibleValues = (1 to 10).toList.filter(_ != 0)
    val coeffs = Seq.fill(numCoeffs)(choose(possibleValues.iterator)).map(_.toDouble).map(BigDecimal(_))
    RealPolynomial(coeffs:_*)
  }

  "A RealPolynomial" should "support addition" in {
    val p1 = RealPolynomial(1,1)
    val p2 = RealPolynomial(1,2,4)
    val p3 = p1 + p2
    assert(p3.coefficients == Seq(2,3,4).map(BigDecimal(_)))
    assert(p3 == RealPolynomial(2,3,4))
  }

  "A Real Polynomial" should "hash almost uniquely" in {
    def randomRealPoly: RealPolynomial = {
      val numCoeffs = choose((1 to 6).iterator)
      val coeffs = Seq.fill(numCoeffs)(Math.random() * 10).map(BigDecimal(_))
      RealPolynomial(coeffs:_*)
    }
    for(size <- 1 to 100){
      val polysA: Seq[RealPolynomial] = (1 to size).map{ _ => randomRealPoly }
      val polysB: Seq[RealPolynomial] = (1 to size).map{ _ => randomRealPoly }
      val s1 = polysA.toSet
      val s2 = polysA.toSet
      val s3 = polysB.toSet
      assert(s1 == s2)
      assert(s1 != s3)
      assert(s2 != s3)
    }
  }

  "A Real Polynomial" should "support negation" in {
    val p1 = RealPolynomial(1,1,1)
    val p2 = -p1
    assert(p2 == p1 * -1)
  }

  "A RealPolynomial" should "support subtraction" in {
    val p1 = RealPolynomial(1,1)
    val p2 = RealPolynomial(1,2,4)
    val p3 = p1 - p2
    assert(p3.coefficients == Seq(0,-1,-4).map(BigDecimal(_)))
  }

  "A RealPolynomial" should "support multiplication" in {
    val p1 = RealPolynomial(3,1)
    val p2 = RealPolynomial(9,-3,1)
    val p3 = p1 * p2
    val p4 = p1 * 2
    assert(p3.coefficients == Seq(27,0,0,1).map(BigDecimal(_)))
    assert(p4 == RealPolynomial(6,2))
  }

  "A RealPolynomial" should "support exponentiation" in {
    val p1 = RealPolynomial(1,1)
    val p0 = p1 ^ 0
    val p2 = p1 ^ 1
    val p3 = p1 ^ 3
    assert(p0 == RealPolynomial.one)
    assert(p2 == p1)
    assert(p2.coefficients == Seq(1,1).map(BigDecimal(_)))
    assert(p3.coefficients == Seq(1,3,3,1).map(BigDecimal(_)))
    for(i <- 0 to 10){
      val pi = p1 ^ i
      val px = pi * p1
      val coeffs = (0 to i).map{ a => i choose a}.map(_.toDouble)
      assert(pi == Array.fill(i)(p1).toList.foldLeft(RealPolynomial.one)((a, b) => a * b))
      assert(px == (p1 ^ (i + 1)))
      assert(pi.coefficients == coeffs)
    }
    // exponentiation should be accurate up to order ~200
    val result = RealPolynomial(1,1,1)^92
    assert(result.coefficients(46).toBigInt().toString() == "60956397035021112188677714778830920")

  }

  "A RealPolynomial" should "support differentiation and antiderivation" in {
    val p1 = RealPolynomial(1,2,1)
    val p2 = p1.derivative
    assert(p2.coefficients == Seq(2,2).map(BigDecimal(_)))
    assert(p2.antiDerivative.coefficients == Seq(0,2,1).map(BigDecimal(_)))
  }

  "A RealPolynomial" should "support division and mod operations" in {
    def testDivision(p1: RealPolynomial, p2: RealPolynomial): Unit = {
      val p3 = RealPolynomial(1)
      val prod1 = p1 * p2
      val prod2 = (p1 * p2) + p3
      val (q1, r1) = prod1/p2
      val (q2, r2) = prod1/p1
      assert(q1 == p1 && r1 == RealPolynomial.zero)
      assert(q2 == p2 && r2 == RealPolynomial.zero)
      assert(prod2%p1 == p3 || prod2%p2 == p3)
      val (q3, r3) = (prod2 - p3)/p2
      val (q4, r4) = (prod2 - p3)/p1
      assert(q3 == p1 && r3 == RealPolynomial.zero)
      assert(q4 == p2 && r4 == RealPolynomial.zero)
    }
    for(_ <- 1 to 100){
      val p1 = randomIntegerPoly
      val p2 = randomIntegerPoly
      testDivision(p1, p2)
      testDivision(p2, p1)
    }
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
    assert(p2.toString == "-x^3+3*x^2-3*x+1")
    assert(p3.toString == "x^3-x^2+x-1")
  }

  "A RealPolynomial" should "compose with other polynomials" in {
    val p1 = RealPolynomial(1, 1)
    val p2 = RealPolynomial(0, 0, 1)
    val p3 = p1 compose p2
    assert(p3 == RealPolynomial(1,2,1))
    assert(p3(3) == 16)
  }

  "RealPolynomial Util" should "compute GCD between polynomials" in {
    for(_ <- 1 to 100){
      val p1 = randomIntegerPoly
      val p2 = randomIntegerPoly
      val p3 = randomIntegerPoly
      val gcdWithin = PolyUtil.GCD(p1, p2)
      val n = ((Math.random() - 0.5) * 100).toInt
      val a = p1 * p3 * n
      val b = p2 * p3 * n
      val gcd = PolyUtil.GCD(a, b)
      assert(gcd == p3 * gcdWithin * n || gcd == p3 * gcdWithin * n * -1)
    }
  }

  "A RealPolynomial" should "Parse a string input construction" in {
    val p1 = RealPolynomial(-16,-24,-4,10,6,1)
    val str1 = "x^5 + 6x^4 + 10x^3 - 4x^2 - 24x - 16"
    assert(RealPolynomial.parse(str1) == p1)
    val str2 = "+x^5 + 6x^4 + 10x^3 - 4x^2 - 24x - 16"
    assert(RealPolynomial.parse(str2) == p1)
    val str3 = "+x^5 + 10x^3 - 4x^2 - 24x - 16 - 3x^5"
    assert(RealPolynomial.parse(str3) == RealPolynomial(-16, -24, -4, 10, 0, -2))
    assert(RealPolynomial.parse("0x^100") == RealPolynomial.zero)
    assert(RealPolynomial.parse("0x^2 + 1x^2") == RealPolynomial(0,0,1))
    val chars = ('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9') ++ List('-', '+', '^', '%', '#', '@', '!', '$')
    val length = choose((1 to 100).iterator)
    for(_ <- 1 to 100) {
      val str = List.fill(length)(choose(chars.iterator)).mkString
      try {
        RealPolynomial.parse(str)
      } catch {
        case e: IllegalArgumentException =>
          assert(e.getMessage.contains(str))
      }
    }
  }

}