import org.bu.metcs789.{ExtendedEuclideanAlgorithm, GCD}
import org.scalatest.{FlatSpec, Matchers}

class EuclideanAlgorithmTest extends FlatSpec with Matchers {
  "Euclidean Algorithm" should "compute correct values" in {
    val (gcd, _) = GCD(614,513)
    assert(gcd == 1)
    assert(ExtendedEuclideanAlgorithm(614, 513) == (-193, 231))
    assert(ExtendedEuclideanAlgorithm(5, 7) == (3,-2))
    assert(ExtendedEuclideanAlgorithm(5, 15) == (1,0))
    assert(ExtendedEuclideanAlgorithm(10, 15) == (-1,1))
    assert(ExtendedEuclideanAlgorithm(9, 16) == (-7,4))
  }
}
