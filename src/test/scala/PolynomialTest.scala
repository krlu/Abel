import org.bu.metcs789.algebraicStructures.rings.polynomials.{PolyUtil, RealPolynomial}
import org.bu.metcs789.factorization.polynomial.Kronecker
import org.scalatest.{FlatSpec, Matchers}

class PolynomialTest extends FlatSpec with Matchers{
  "A RealPolynomial" should "support addition" in {
    val p1 = RealPolynomial(1,1)
    val p2 = RealPolynomial(1,2,4)
    val p3 = p1 + p2
    assert(p3.coefficients == Seq(2,3,4))
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
    assert(p3.coefficients == Seq(27,0,0,1))
  }

  "A RealPolynomial" should "support exponentiation" in {
    val p1 = RealPolynomial(1,1)
    val p2 = p1 ^ 3
    val p3 = p1 ^ 1
    assert(p3.coefficients == Seq(1,1))
    assert(p2.coefficients == Seq(1,3,3,1))
  }

  "A RealPolynomial" should "support differentiation and antiderivation" in {
    val p1 = RealPolynomial(1,2,1)
    val p2 = p1.derivative
    assert(p2.coefficients == Seq(2,2))
    assert(p2.antiDerivative.coefficients == Seq(0,2,1))
  }

  "A RealPolynomial" should "support division and mod operations" in {
    val x = RealPolynomial(-1,0,0,0,1)
    val y = RealPolynomial(1,1)
    val (quotient, remainder) = x / y
    val modY = x%y

    assert(quotient == RealPolynomial(-1,1,-1,1))
    assert(RealPolynomial(quotient.coefficients:_*).toString() == "x^3 + (-1.0)x^2 + x + (-1.0)")
    assert(quotient*y == x)
    assert(remainder == modY)
    assert(remainder == RealPolynomial.zero)
  }

  "A RealPolynomial" should "support toString" in {
    val p1 = RealPolynomial(1,-1)
    val p2 = p1 ^ 3
    assert(RealPolynomial(p2.coefficients:_*).toString == "(-1.0)x^3 + (3.0)x^2 + (-3.0)x + (1.0)")
    assert(RealPolynomial(p2.coefficients:_*) == p1 * p1 * p1)
  }

  "A RealPolynomial" should "compose with other polynomials" in {
    val p1 = RealPolynomial(1, 1)
    val p2 = RealPolynomial(0, 0, 1)
    val p3 = p1 compose p2
    assert( p3 == RealPolynomial(1,2,1))
    assert(p3(3) == 16)
  }

  "RealPolynomial Util" should "compute GCD between polynomials" in {
    val p1 = RealPolynomial(0,1) * RealPolynomial(1,1)
    val p2 = RealPolynomial(1,0,1) * RealPolynomial(1,1)
    assert(PolyUtil.GCD(p1, p2) == RealPolynomial(1,1))

//    val p3 = RealPolynomial(-1,1) * RealPolynomial(1,1)
//    val p4 = RealPolynomial(1,1) ^ 2
//    assert(PolyUtil.GCD(p3, p4) == RealPolynomial(1,1))
  }

  "Kronecker's Method" should "Factor Polynomials" in {
    val p1 = RealPolynomial(-1, 0, 0, 0, 1)
    val p2 = RealPolynomial(-1, 0, 1)

    val factors1 = Set(RealPolynomial(1, 1), RealPolynomial(-1, 1), RealPolynomial(1, 0, 1))
    val factors2 = Set(RealPolynomial(1, 1), RealPolynomial(-1, 1))
    assert(Kronecker(p1).toSet == factors1)
    val x = factors1.reduce((a, b) => a * b)
    assert(x == p1)

    assert(Kronecker(p2).toSet == factors2)
    assert(factors2.reduce((a, b) => a * b) == p2)

    assert(Kronecker(RealPolynomial(1, 0, 1)) == Seq(RealPolynomial(1, 0, 1)))
    for (i <- 1 to 3) {
      val p = RealPolynomial(1, 1) ^ i
      val factors = Kronecker(p)
      assert(factors.size == i)
      assert(factors.toSet == Set(RealPolynomial(1,1)))
    }
    for (i <- -10 to 10) {
      val pi = RealPolynomial(i)
      assert(Kronecker(pi) == Seq(pi))
    }
  }
}
