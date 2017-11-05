package org.bu.metcs789

import org.bu.metcs789.Basics.NthRoot

/**
  * Implementation of Oblivious transfer protocol
  */
class ObliviousTransfer {

}

/**
  * @param n - the modulus, and n = pq for some primes p and q
  */
private class FindPQ(n: Int) extends (Int => (Int, Int)){
  override def apply(someSquare: Int): (Int, Int) = {
    val roots: Seq[Int] = NthRoot(n)(someSquare, 2)

    (0,0)
  }
}
