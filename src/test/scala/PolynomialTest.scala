import org.bu.metcs789.polynomials.{PolyUtil, Polynomial}
import org.scalatest.{FlatSpec, Matchers}

class PolynomialTest extends FlatSpec with Matchers{
  "A Polynomial" should "support binary addition" in {
    val p1 = Polynomial(1,1)
    val p2 = Polynomial(1,2,4)
    val p3 = p1 + p2
    assert(p3.coefficients == Seq(2,3,4))
  }
  "A Polynomial" should "support binary subtraction" in {
    val p1 = Polynomial(1,1)
    val p2 = Polynomial(1,2,4)
    val p3 = p1 - p2
    assert(p3.coefficients == Seq(0,-1,-4))
  }

  "A Polynomial" should "support binary multiplication" in {
    val p1 = Polynomial(3,1)
    val p2 = Polynomial(9,-3,1)
    val p3 = p1 * p2
    assert(p3.coefficients == Seq(27,0,0,1))
  }

  "A Polynomial" should "support exponentiation" in {
    val p1 = Polynomial(1,1)
    val p2 = p1 ^ 3
    val p3 = p1 ^ 1
    assert(p3.coefficients == Seq(1,1))
    assert(p2.coefficients == Seq(1,3,3,1))
  }

  "A Polynomial" should "support differentiation and antiderivation" in {
    val p1 = Polynomial(1,2,1)
    val p2 = p1.derivative
    assert(p2.coefficients == Seq(2,2))
    assert(p2.antiDerivative.coefficients == Seq(0,2,1))
  }

  "A Polynomial" should "support division and mod operations" in {
    val x = Polynomial(-1,0,0,0,1)
    val y = Polynomial(1,1)
    val (quotient, remainder) = x / y
    val modY = x%y
    assert(quotient == Polynomial(-1,1,-1,1))
    assert(quotient.toString() == "x^3 + (-1.0)x^2 + x + (-1.0)")
    assert(quotient*y == x)
    assert(remainder == modY)
    assert(remainder == Polynomial.zero)
  }

  "A Polynomial" should "support toString" in {
    val p1 = Polynomial(1,-1)
    val p2 = p1 ^ 3
    assert(p2.toString == "(-1.0)x^3 + 3.0x^2 + (-3.0)x + 1.0")
  }

  "Polynomial Util" should "compute GCD between polynomials" in {
    val p1 = Polynomial(0,1) * Polynomial(1,1)
    val p2 = Polynomial(1,0,1) * Polynomial(1,1)
    assert(PolyUtil.GCD(p1, p2) == Polynomial(1,1))
  }

  "Kronecker's Method" should "Factor Polynomials" in {
    val p1 = Polynomial(-1,0,0,0,1)
    assert(PolyUtil.kroneckerFactorization(p1).toSet == Set(Polynomial(1,1), Polynomial(-1,1), Polynomial(1,0,1)))
  }
}
