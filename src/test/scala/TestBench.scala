import org.bu.metcs789.algebraicStructures.rings.polynomials.RealPolynomial

object TestBench {
  def main(args: Array[String]): Unit = {
    //    val p1 = RealPolynomial(-16, -24, -4, 10, 6, 1)
    //    val p2 = p1.derivative
    //    println(p1)
    //    println(p2)
    //    val (q,r) = p1/p2
    //    println(q * p2 + r)
    //    println(PolyUtil.GCD(p1, p2))
    val p1 = RealPolynomial(-1,0,1)
    val p2 = RealPolynomial(2,2)
    val (a, b) = p2.reduceCoeffs
    println(a,b)
    println(p1)
    println(p2/RealPolynomial(2))
    println(p1 % p2)
    //    println(PolyUtil.GCD(p1, a))
  }
}
