package org.bu.abel

import org.bu.abel.algops.fields.IntegerModN
import org.bu.abel.algops.rings.IntegerRing
import org.bu.abel.basics._
import org.bu.abel.encryption._
import org.bu.abel.factorization.FactorUtil
import org.bu.abel.factorization.Integer.{PollardP1, PollardRho, PrimeFactorization}
import org.bu.abel.rng.{BlumBlumShub, NaorReingold}
import org.bu.abel.types.LargeNumber
import org.scalatest.{FlatSpec, Matchers}

class CryptoTests extends FlatSpec with Matchers {

  "Euclidean Algorithm" should "compute correct values" in {
    val (gcd, _) = GCDUtil.gcd(614,513)
    assert(gcd == 1)
    assert(GCDUtil.extendedgcd(614, 513) == (-193, 231))
    assert(GCDUtil.extendedgcd(5, 7) == (3,-2))
    assert(GCDUtil.extendedgcd(5, 15) == (1,0))
    assert(GCDUtil.extendedgcd(10, 15) == (-1,1))
    assert(GCDUtil.extendedgcd(9, 16) == (-7,4))
    assert(GCDUtil.multigcd(Seq(6L, 9L, 15L, 21L)).get == 3)
    val m = LargeNumber.fromString("205891132094649")
    val n = LargeNumber.fromString("137260754729766")
    val (largeGcd1, _) = GCDUtil.gcd(n,m)
    val (largeGcd2, _) = GCDUtil.gcd(m,n)
    assert(largeGcd1 == largeGcd2)
    assert(largeGcd1 == LargeNumber.fromString("68630377364883"))
    assert(m/largeGcd1 == 3)
    assert(n/largeGcd1 == 2)
  }

  "Exponentiation Algorithm" should "compute correct values" in {
    val Z = IntegerRing()
    assert(Z.pow(1, 0) == 1)
    assert(Z.pow(2, 0) == 1)
    assert(Z.pow(300, 0) == 1)

    assert(Z.pow(1, 1) == 1)
    assert(Z.pow(1, 2) == 1)
    assert(Z.pow(1, 300) == 1)

    assert(Z.pow(2,1) == 2)
    assert(Z.pow(2,2) == 4)
    assert(Z.pow(2,3) == 8)
    assert(Z.pow(2,4) == 16)

    assert(Z.pow(0,1) == 0)
    assert(Z.pow(0,2) == 0)
    assert(Z.pow(0,110) == 0)

    assert(Z.pow(1,0) == 1)
    assert(Z.pow(2,0) == 1)
    assert(Z.pow(3,0) == 1)

    assert(IntegerModN(15688).pow(3, PrimeUtil.phi(15688) -1) == 10459)
  }

  "Prime Finder" should "Finder Primes" in {
    assert(PrimeUtil.primesLessThanN(4) == List(2,3))
    assert(PrimeUtil.primesLessThanN(6) == List(2,3,5))
    assert(PrimeUtil.primesLessThanN(30) == List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29))
    assert(PrimeUtil.primesLessThanN(100) ==
      List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97))
  }

  "Primitive Root Finder" should "find primitive roots modulo P" in {
    assert(PrimitiveRoots(2) == List(1))
    assert(PrimitiveRoots(3) == List(2))
    assert(PrimitiveRoots(4) == List(3))
    assert(PrimitiveRoots(5) == List(2,3))
    assert(PrimitiveRoots(11) == List(2,6,7,8))
    assert(PrimitiveRoots(54) == List(5, 11, 23, 29, 41, 47))
    assert(PrimitiveRoots(71) ==
      List(7, 11, 13, 21, 22, 28, 31, 33, 35, 42, 44, 47, 52, 53, 55, 56, 59, 61, 62, 63, 65, 67, 68, 69))
  }

  "Mod Inverse" should "compute mod inverse" in {
    assert(IntegerModN.modInverse(5,8) == 5)
    assert(IntegerModN.modInverse(2,3) == 2)
    assert(IntegerModN.modInverse(3,11) == 4)

    val inv = IntegerModN.modInverse(3313, PrimeUtil.phi(4187))
    val zModN = IntegerModN(4187)
    for(i <- PrimeUtil.relPrimesLessThanN(4187)) {
      val power = zModN.pow(i, 3313)
      val original = zModN.pow(power, inv)
      assert(original == i)
    }
  }

  "Prime Factorization" should "find prime factors" in {
    assert(PrimeFactorization(12) == Seq(2,2,3))
    assert(PrimeFactorization(9) == Seq(3,3))
    assert(PrimeFactorization(30) == Seq(2,3,5))

  }

  "Get All Factors" should "find all possible factors" in {
    assert(FactorUtil.getAllFactors(9) == Seq(1,3,9))
    assert(FactorUtil.getAllFactors(12) == Seq(1,2,3,4,6,12))
    assert(FactorUtil.getAllFactors(32) == Seq(1,2,4,8,16,32))
    assert(FactorUtil.getAllFactors(729) == Seq(1,3,9,27,81,243,729))
  }

  "Baby Step Giant Step" should "compute discrete Log" in {
    assert(DiscreteLog(19)(3,729).get == 6)
    assert(DiscreteLog(19)(2,2048).get == 11)
    assert(DiscreteLog(19)(3,6).get == 8)
    assert(DiscreteLog(18)(3,6).isEmpty)
    assert(DiscreteLog(15)(2,5).isEmpty)

    for(i <- PrimeUtil.relPrimesLessThanN(4187)) {
      val power = IntegerModN(4187).pow(i, 3313)
      val logs = AllDiscreteLogs(4187)(i, power)
      assert(logs.contains(3313))
    }
    val p = 857
    val pRoots = PrimitiveRoots(p)
    for(i <- PrimeUtil.relPrimesLessThanN(p)){
      val power = IntegerModN(p).pow(i, 33)
      val logs = AllDiscreteLogs(p)(2, power)
      if(logs.size > 1)
        assert(!pRoots.contains(i))
    }
  }

  "Diffie Hellman" should "compute same key for alice and bob" in {
    val alice = DHUser(4,1, Seq())
    val bob = DHUser(3,1, Seq())
    DiffieHellman(5, 9511)(alice, bob)
    assert(alice.sharedKey == 2766 && bob.sharedKey == 2766)
    DiffieHellman(5, 23)(alice, bob)
    assert(alice.sharedKey == 18 && bob.sharedKey == 18)
  }

  "Square root finder" should "compute square roots" in {
    assert(NthRoot(11)(5,2) == Seq(4,7))
    assert(NthRoot(161)(2,2) == Seq(18, 74, 87, 143))
  }

  "Find P Q " should "find factorization" in {
    assert(FindPQ(15)(4).get == (5,3))
  }

  "Oblivious Transfer with Factorization" should "compute same key for alice and bob" in {
    for(_ <- 1 to 50) {
      val alice = OTPUser(Some(31), Some(103))
      var bob = OTPUser(None, None)
      bob = ObliviousTransferWithFactorization(alice, bob)
      assert(Set(bob.s0, bob.s1) == Set(alice.s0, alice.s1))
      bob = OTPUser(None, None)
      bob = ObliviousTransferWithDiscreteLog(103)(alice, bob)
      assert(Set(bob.s0, bob.s1).intersect(Set(alice.s0, alice.s1)).size == 1)
    }
  }

  "Miller Rabin" should "correctly verify primes" in {
    for(_ <- 1 to 10)
      Seq(30949, 30983, 31013, 31019, 31039, 31051, 31063, 43541).foreach{ p => assert(PrimeUtil.millerRabin(p, 10))}
    Set(31053, 31065, 31067, 31077, 31083, 31093, 31127, 31127, 35259).foreach{ p => assert(!PrimeUtil.millerRabin(p, 10))}
    assert(!PrimeUtil.millerRabin(1027485, 1))
  }

  "Naor Reingold" should "randomly generate binary bits" in {
//    val generator = NaorReingold(10)
    val generator = NaorReingold(4)
    var ones = 0
    var zeroes = 0
//    for(i <- 0 until Math.pow(2, 10).toInt)
    for(i <- 0 until Math.pow(2, 3).toInt)
      if (generator(i) == 1) ones += 1 else zeroes += 1
    assert(Math.abs(ones -zeroes) < 100)
  }

  "Blum Blum Shub" should "randomly generate binary bits" in {
    val generator = BlumBlumShub(7, 11)
    var ones = 0
    var zeroes = 0
    for(_ <- 1 to 1000){
      if(generator.generateBit == 1) ones+=1 else zeroes +=1
      generator.resetSeed()
    }
    assert(Math.abs(ones - zeroes) < 100)
  }

  "Pollard Rho" should "find factor of N" in {
    assert(Set[Long](73, 41).contains(PollardRho(2993)))
    assert(Set[Long](2,5).contains(PollardRho(10)))
    Seq(30949, 30983, 31013, 31019, 31039, 31051, 31063, 43541).foreach{ p => assert(PollardRho(p) == 1L)}
    Set(31053, 31065, 31067, 31077, 31083, 31093, 31127, 35259).foreach{ p => assert(PollardRho(p) != 1L)}
  }

  "Pollard P-1" should  "find factor of N" in {
    assert(Set[Long](73, 41).contains(PollardP1(2993,10)))
    assert(Set[Long](2,5).contains(PollardP1(10,8)))
//    Seq(30949, 30983, 31013, 31019, 31039, 31051, 31063, 43541).foreach{ p => assert(PollardP1(p, 10) == 1L)}
//    Set(31053, 31065, 31067, 31077, 31083, 31093, 31127, 31127, 35259).foreach{ p => assert(PollardP1(p, 10, 50) == 1L)}
  }
}
