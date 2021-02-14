package org.bu.abel.encryption

import org.bu.abel.algops.fields.IntegerModN
import org.bu.abel.basics._
import org.bu.abel.factorization.Integer.PollardRho

object EncryptionExperiments{

  def main(args: Array[String]) {
   
    println("************************************ RSA Experiment******************************************")

    println("Encrypting Message as Alice")
    val (nAlice, m, gAlice) = (4187, 3313, 321)
    println("Let n = 4187, g = 3313, message = 321")
    println(s"Alice sends m^g mod n = ${IntegerModN(nAlice).pow(m,gAlice)}")
    println()

    println("Sending Public Key as Bob")
    val (p, q, gBob) = (73, 79, 5)
    require(PrimeUtil.isPrime(p), PrimeUtil.isPrime(q))
    val nBob = p*q
    println(s"Bob sends n = $nBob and g = $gBob")
    println("Decrypting Message from Alice")
    RSADecrypt(p * q, 5, 3560, 72 * 78, Bob)
    println()

    println("Breaking RSA as Eve")
    RSADecryptEve(9313, 13, 3560)
    println()

    println("********************************** El Gamal Experiment****************************************")

    println("Sending Message as Alice")
    ElGamalAlice(857, 125, 100)
    println("Decrypting Message from Bob")
    val g_ab = IntegerModN(857).pow(674, 100)
    val g_ab_inv = IntegerModN.modInverse(g_ab, 857)
    println(s"Alice computes g^(-ab) = $g_ab_inv and m = ${g_ab_inv * 120 % 857}")
    println()

    println(s"Sending message as Bob: ")
    ElGamalBob(941, 141, 788, 142, 729)
    println()

    println("Breaking El Gamal as Eve")
    ElGamalEve(1559, 569, 1372, 575, 471)
  }

  private def RSADecrypt(n: Int, g: Int, y: Int, phi: Int, person: Person): Unit ={
    val  g_inv = IntegerModN.modInverse(g, phi)
    val name = person match {
      case Alice => throw new IllegalArgumentException("Cannot pass alice to this method!")
      case Bob => "Bob"
      case Eve => "Ever"
    }
    println(s"$name Decrypts by getting y^(-g) = ${IntegerModN(n).pow(y, g_inv)}")
  }

  private def RSADecryptEve(n: Int, g: Int, y: Int): Unit ={
    val p = PollardRho(n).toInt
    val q = n/p
    println(s"Eve finds p = $p and q = $q")
    RSADecrypt(n, g, y, (p-1)*(q-1), Eve)
  }

  private def ElGamalAlice(p: Int, g: Int, a: Int): Unit ={
    println(s"Let p = $p, g = $g, a = $a")
    println(s"Alice sends  p = $p, g = $g, g^a = ${IntegerModN(p).pow(g,a)}")
  }

  private def ElGamalEve(p: Int, g: Int, g_a: Int, g_b: Int, m_g_ab: Int): Unit ={
    val log = DiscreteLog(1559)(569, 1372)
    val a = DiscreteLog(p)(g, g_a).get
    println(s"a = Log_${g}_$g_a = $a")
    println(s"Verifying that $g^${log.get} = ${IntegerModN(p).pow(g, log.get)}")
    val g_ab = IntegerModN(p).pow(g_b, a)
    println(s"g^ab = $g_ab")
    val g_ab_inv = IntegerModN.modInverse(g_ab, p)
    println(s"g^(-ab) = $g_ab_inv")
    println(s"m = ${g_ab_inv * m_g_ab % p}")
  }

  private def ElGamalBob(p: Int, g: Int, g_a: Int, b: Int, m : Int){
    val g_ab = IntegerModN(p).pow(g_a, b)
    println(s"Let p = $p , g = $g,  b = $b, m = $m ")
    println(s"g^b = ${IntegerModN(p).pow(g,b)}")
    println(s"m*(g^(ab)) = ${(m * g_ab) % p}")
  }
}

private trait Person
private case object Alice extends Person
private case object Bob extends Person
private case object Eve extends Person
