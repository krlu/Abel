package org.bu.abel.encryption

import org.bu.abel.basics._
import org.bu.abel._
import org.bu.abel.algops.fields.IntegerModN

trait ObliviousTransfer extends ((OTPUser, OTPUser) => OTPUser)

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
        val bobNumSq: Double = IntegerModN(n).pow(bobNumber, 2)
        val optPQPair = FindPQ(n)(bobNumSq.toLong)
        optPQPair match {
          // don't know which is P or Q
          case Some((factor1, factor2)) => bob.copy(s0 = Some(factor1), s1 = Some(factor2))
          case None => bob
        }
      case _ => bob
    }
  }
  /**
    * @param n - the modulus
    */
  private def generateRandomRelPrime(n: Long): Long ={
    var number = ((Math.random() * n).toLong + 1) % n
    while(GCDUtil.gcd(number, n)._1 != 1)
      number = ((Math.random() * n).toLong + 1) % n
    number
  }
}

/**
  * Implementation of Oblivious transfer protocol with discrete logarithm
  *
  * @param modulus - some large prime
  */
protected class ObliviousTransferWithDiscreteLog(modulus: Int) extends ObliviousTransfer{
  require(modulus > 1)
  val fastExpWithMod: IntegerModN = IntegerModN(modulus)
  override def apply(alice: OTPUser, bob: OTPUser): OTPUser = {
    var biInvXY0, biInvXY1, t0Copy, t1Copy, m0, m1 = -1L
    do {
      val elements = 2 until modulus - 1
      // alice
      val g = choose(PrimitiveRoots(modulus).iterator)
      val c = choose(elements.iterator)

      // bob
      val i = choose(Set(0, 1).iterator)
      val x = choose(elements.iterator)
      val bi = fastExpWithMod.pow(g, x)
      val biInv = (c * IntegerModN.modInverse(bi, modulus)) % modulus
      val B = Map(i -> bi, (1 - i) -> biInv)

      // alice
      val y0 = choose(elements.iterator)
      val y1 = choose(elements.iterator)
      val a0 = fastExpWithMod.pow(g, y0)
      val t0 = fastExpWithMod.pow(B(0), y0)
      val a1 = fastExpWithMod.pow(g, y1)
      val t1 = fastExpWithMod.pow(B(1), y1)
      m0 = alice.s0.get ^ t0
      m1 = alice.s1.get ^ t1

      // bob
      t0Copy = fastExpWithMod.pow(a0, x)
      t1Copy = fastExpWithMod.pow(a1, x)
      biInvXY0 = fastExpWithMod.pow(biInv, y0)
      biInvXY1 = fastExpWithMod.pow(biInv, y1)
//      if (Set(m0 ^ t0Copy, m1 ^ t1Copy) == Set(alice.s0.get, alice.s1.get))
//        println(s"c =$c, g=$g, x=$x, B=$B, y0=$y0, i=$i, y1=$y1, t0=$t0, " + s"t1=$t1, a0^x=$t0Copy, a1^x=$t1Copy, a0=$a0, a1=$a1")
    }while(t0Copy == biInvXY0 || t1Copy == biInvXY1)
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
protected class FindPQ(n: Long) extends (Long => Option[(Long, Long)]){
  override def apply(v1: Long): Option[(Long, Long)] = {
    val roots: Seq[Long] = NthRoot(n)(v1, 2)
    require(roots.size == 4)
    for(root <- roots) {
      if (root != v1 && v1 + root != n) {
        val x1 = roots.head
        val y1 = roots(1)
        val prod1 = ((x1 - y1) % n + n ) % n
        val prod2 = ((x1 + y1) % n + n ) % n
        return Some((GCDUtil.gcd(prod1, n)._1,GCDUtil.gcd(prod2, n)._1))
      }
    }
    None
  }
}

object FindPQ {
  def apply(n: Long): FindPQ = new FindPQ(n)
}
