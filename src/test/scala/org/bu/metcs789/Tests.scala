package org.bu.metcs789

import org.bu.metcs789.basics._
import org.bu.metcs789.encryption._
import org.bu.metcs789.factorization.{PollardP1, PollardRho, PrimeFactorization}
import org.bu.metcs789.rng.{BlumBlumShub, NaorReingold}
import org.scalatest.{FlatSpec, Matchers}

class Tests extends FlatSpec with Matchers {
  "Euclidean Algorithm" should "compute correct values" in {
    val (gcd, _) = GCD(614,513)
    assert(gcd == 1)
    assert(ExtendedGCD(614, 513) == (-193, 231))
    assert(ExtendedGCD(5, 7) == (3,-2))
    assert(ExtendedGCD(5, 15) == (1,0))
    assert(ExtendedGCD(10, 15) == (-1,1))
    assert(ExtendedGCD(9, 16) == (-7,4))
    assert(MultiGCD(Seq(6L, 9L, 15L, 21L)).get == 3)
  }
  "Exponentiation Algorithm" should "compute correct values" in {
    assert(FastExp(1, 0) == 1)
    assert(FastExp(2, 0) == 1)
    assert(FastExp(300, 0) == 1)

    assert(FastExp(1, 1) == 1)
    assert(FastExp(1, 2) == 1)
    assert(FastExp(1, 300) == 1)

    assert(FastExp(2,1) == 2)
    assert(FastExp(2,2) == 4)
    assert(FastExp(2,3) == 8)
    assert(FastExp(2,4) == 16)

    assert(FastExp(0,1) == 0)
    assert(FastExp(0,2) == 0)
    assert(FastExp(0,110) == 0)

    assert(FastExp(1,0) == 1)
    assert(FastExp(2,0) == 1)
    assert(FastExp(3,0) == 1)

//    assert(FastExp(2,-1) == 0.5)
//    assert(FastExp(2,-2) == 0.25)
//    assert(FastExp(2,-3) == 0.125)
//    assert(Math.abs(FastExp(Math.sqrt(2).toLong, 2) - 2) < 0.00000004)

    assert(FastExpWithMod(15688)(3, Phi(15688) -1) == 10459)
  }

  "Prime Finder" should "Finder Primes" in {
    assert(PrimesLessThanN(4) == List(2,3))
    assert(PrimesLessThanN(6) == List(2,3,5))
    assert(PrimesLessThanN(30) == List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29))
    assert(PrimesLessThanN(100) ==
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
    assert(ModInverse(5,8) == 5)
    assert(ModInverse(2,3) == 2)
    assert(ModInverse(3,11) == 4)

    val inv = ModInverse(3313, Phi(4187))
    for(i <- RelPrimesLessThanN(4187)) {
      val power = FastExpWithMod(4187)(i, 3313)
      val original = FastExpWithMod(4187)(power, inv)
      assert(original == i)
    }
  }

  "Baby Step Giant Step" should "compute discrete Log" in {
    assert(DiscreteLog(19)(3,729).get == 6)
    assert(DiscreteLog(19)(2,2048).get == 11)
    assert(DiscreteLog(19)(3,6).get == 8)
    assert(DiscreteLog(18)(3,6).isEmpty)
    assert(DiscreteLog(15)(2,5).isEmpty)

    for(i <- RelPrimesLessThanN(4187)) {
      val power = FastExpWithMod(4187)(i, 3313)
      val logs = AllDiscreteLogs(4187)(i, power)
      assert(logs.contains(3313))
    }
    val p = 857
    val pRoots = PrimitiveRoots(p)
    for(i <- RelPrimesLessThanN(p)){
      val power = FastExpWithMod(p)(i, 33)
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
    for(_ <- 1 to 10){
      Seq(30949, 30983, 31013, 31019, 31039, 31051, 31063, 43541).foreach{ p => assert(MillerRabin(p, 10))}
    }
    Set(31053, 31065, 31067, 31077, 31083, 31093, 31127, 31127, 35259).foreach{ p => assert(!MillerRabin(p, 10))}
    assert(!MillerRabin(1027485, 1))
  }

  "Naor Reingold" should "randomly generate binary bits" in {
    val generator = NaorReingold(10)
    var ones = 0
    var zeroes = 0
    for(i <- 0 until Math.pow(2, 10).toInt)
      if(generator(i) == 1) ones+=1 else zeroes +=1
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
//    println(zeroes, ones)
    assert(Math.abs(ones - zeroes) < 100)
  }

  "Prime Facotrization" should "Find all prime factors of N" in {
    assert(Set[Long](2,3,5) == PrimeFactorization(30).toSet)
  }

  "Pollard Rho" should "find factor of N" in {
    assert(Set[Long](73, 41).contains(PollardRho(2993)))
    assert(Set[Long](2,5).contains(PollardRho(10)))
    Seq(30949, 30983, 31013, 31019, 31039, 31051, 31063, 43541).foreach{ p => assert(PollardRho(p) == 1L)}
    Set(31053, 31065, 31067, 31077, 31083, 31093, 31127, 31127, 35259).foreach{ p => assert(PollardRho(p) == 1L)}
    assert(Set[Long](41, 73).contains(PollardP1(2993, 30)))
  }
  "Pollard P-1" should  "find factor of N" in {
    assert(Set[Long](73, 41).contains(PollardP1(2993,30)))
    assert(Set[Long](2,5).contains(PollardP1(10,30)))
    Seq(30949, 30983, 31013, 31019, 31039, 31051, 31063, 43541).foreach{ p => assert(PollardP1(p, 10) == 1L)}
    Set(31053, 31065, 31067, 31077, 31083, 31093, 31127, 31127, 35259).foreach{ p => assert(PollardP1(p, 10, 50) == 1L)}
  }
}
