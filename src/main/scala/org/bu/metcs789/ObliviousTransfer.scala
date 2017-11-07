package org.bu.metcs789

import org.bu.metcs789.Basics._

trait ObliviousTransfer  extends ((OTPUser, OTPUser) => OTPUser){
  /**
    * @param n - the modulus
    */
  protected def generateRandomRelPrime(n: Long): Long ={
    var number = ((Math.random() * n).toLong + 1) % n
    while(GCD(number, n)._1 != 1)
      number = ((Math.random() * n).toLong + 1) % n
    number
  }
}

/**
  * Implementation of Oblivious transfer protocol with factorization
  * Alice's secrets are s1 = p and s2 = q, p and q are prime
  * pq = n, bob doesn't know pq so they are both none initially
  */
object ObliviousTransferWithFactorization extends ObliviousTransfer{
  override def apply(alice: OTPUser, bob: OTPUser): OTPUser = {
    require(bob.s0.isEmpty && bob.s1.isEmpty)
    (alice.s0, alice.s1) match {
      case (Some(p), Some(q)) =>
        val n = p*q
        val bobNumber = generateRandomRelPrime(n)
        val bobNumSq = FastExpWithMod(n)(bobNumber, 2)
        val roots: Seq[Long] = NthRoot(n)(bobNumSq.toInt, 2)
        require(roots.size == 4)
        for(root <- roots){
          if(root != bobNumber && bobNumber + root != n){
            val x1 = bobNumber
            val y1 = root
            val prod1 = ((x1 - y1) % n + n ) % n
            val prod2 = ((x1 + y1) % n + n ) % n
            val factor1 = GCD(prod1, n)._1
            val factor2 = GCD(prod2, n)._1
            // bob now knows p and q
            return bob.copy(s0 = Some(factor1), s1 = Some(factor2))
          }
        }
        bob
      case _ => bob
    }
  }
}

/**
  * Implementation of Oblivious transfer protocol with discrete logarithm
 *
  * @param modulus - some large prime
  */
protected class ObliviousTransferWithDiscreteLog(modulus: Int) extends ObliviousTransfer{
  require(modulus > 1)
  val fastExpWithMod = FastExpWithMod(modulus)
  override def apply(alice: OTPUser, bob: OTPUser): OTPUser = {
    // alice
    val g = PrimitiveRoots(modulus).head
    val c = (Math.random() * (modulus - 1)).toLong
    // bob
    val i = if(Math.random() < 0.5) 0 else 1
    val x = (Math.random() * (modulus - 1)).toLong
    val bi = fastExpWithMod(g, x).toLong
    val biInv = (ModInverse(bi, modulus) * c) % modulus
    val B = Map(i -> bi, (1-i) -> biInv)
    // alice
    val y0 = (Math.random() * (modulus - 1)).toLong
    val y1 = (Math.random() * (modulus - 1)).toLong
    val a0 = fastExpWithMod(g, y0).toLong
    val a1 = fastExpWithMod(g, y1).toLong
    val t0 = fastExpWithMod(B(0), y0).toLong
    val t1 = fastExpWithMod(B(1), y1).toLong
    val m0 = alice.s0.get ^ t0
    val m1 = alice.s1.get ^ t1
    // bob
    val t0Copy = fastExpWithMod(a0, x).toLong
    val t1Copy = fastExpWithMod(a1, x).toLong
    bob.copy(s0 = Some(m0 ^ t0Copy), s1 = Some(m1 ^ t1Copy))
  }
}

object ObliviousTransferWithDiscreteLog{
  def apply(modulus: Int): ObliviousTransferWithDiscreteLog = new ObliviousTransferWithDiscreteLog(modulus)
}

/**
  * @param s0 - secret 1
  * @param s1 - secret 2
  */
case class OTPUser(s0: Option[Long], s1: Option[Long])
/**
  * @param n - the modulus, and n = pq for some primes p and q
  */
protected class FindPQ(n: Long) extends (Long => (Long, Long)){
  override def apply(v1: Long): (Long, Long) = {
    val roots: Seq[Long] = NthRoot(n)(v1, 2)
    require(roots.size == 4)
    val x1 = roots.head
    val y1 = roots(1)
    val prod1 = ((x1 - y1) % n + n ) % n
    val prod2 = ((x1 + y1) % n + n ) % n
    (GCD(prod1, n)._1,GCD(prod2, n)._1)
  }
}

object FindPQ {
  def apply(n: Long): FindPQ = new FindPQ(n)
}
