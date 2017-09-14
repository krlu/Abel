import org.bu.metcs789.{ExtendedEuclideanAlgorithm, GCD}
import org.scalatest.{FlatSpec, Matchers}

class EuclideanAlgorithmTest extends FlatSpec with Matchers {
  "Euclidean Algorithm" should "compute correct values" in {
    val (gcd, _) = GCD(614,513)
    assert(gcd == 1)
    assert(ExtendedEuclideanAlgorithm(614, 513) == (-193, 231))
  }
}
