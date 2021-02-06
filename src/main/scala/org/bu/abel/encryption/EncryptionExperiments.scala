package org.bu.abel.encryption

import org.bu.abel.basics._
import org.bu.abel.factorization.Integer.PollardRho

object EncryptionExperiments{

  def main(args: Array[String]) {
    println("************************************ RSA Experiment******************************************")

    println("Encrypting Message as Alice")
    println("Let n = 4187, g = 3313, message = 321")
    RSAAlice(4187, 3313, 321)
    println()

    println("Sending Public Key as Bob")
    RSABob(73, 79, 5)
    println("Decrypting Message from Alice")
    RSADecrypt(73 * 79, 5, 3560, 72 * 78, Bob)
    println()

    println("Breaking RSA as Eve")
    RSADecryptEve(9313, 13, 3560)
    println()

    println("********************************** El Gamal Experiment****************************************")

    println("Sending Message as Alice")
    ElGamalAlice(857, 125, 100)
    println("Decrypting Message from Bob")
    val g_ab = FastExpWithMod(857)(674, 100)
    val g_ab_inv = ModInverse(g_ab, 857)
    println(s"Alice computes g^(-ab) = $g_ab_inv and m = ${g_ab_inv * 120 % 857}")
    println()

    println(s"Sending message as Bob: ")
    ElGamalBob(941, 141, 788, 142, 729)
    println()

    println("Breaking El Gamal as Eve")
    ElGamalEve(1559, 569, 1372, 575, 471)
  }

  private def RSAAlice(n: Int, g: Int, m: Int): Unit = {
    println(s"Alice sends m^g = ${FastExpWithMod(n)(m,g)}")
  }

  private def RSABob(p: Int, q: Int, g: Int): Unit ={
    require(IsPrime(p), IsPrime(q))
    val n = p*q
    println(s"Bob sends n = $n and g = $g")
  }

  private def RSADecrypt(n: Int, g: Int, y: Int, phi: Int, person: Person): Unit ={
    val  g_inv= ModInverse(g, phi)
    val name = person match {
      case Alice => throw new IllegalArgumentException("Cannot pass alice to this method!")
      case Bob => "Bob"
      case Eve => "Ever"
    }
    println(s"$name Decrypts by getting y^(-g) = ${FastExpWithMod(n)(y, g_inv)}")
  }

  private def RSADecryptEve(n: Int, g: Int, y: Int): Unit ={
    val p = PollardRho(n).toInt
    val q = n/p
    println(s"Eve finds p = $p and q = $q")
    RSADecrypt(n, g, y, (p-1)*(q-1), Eve)
  }

  private def ElGamalAlice(p: Int, g: Int, a: Int): Unit ={
    println(s"Let p = $p, g = $g, a = $a")
    println(s"Alice sends  p = $p, g = $g, g^a = ${FastExpWithMod(p)(g,a)}")
  }

  private def ElGamalEve(p: Int, g: Int, g_a: Int, g_b: Int, m_g_ab: Int): Unit ={
    val log = DiscreteLog(1559)(569, 1372)
    val a = DiscreteLog(p)(g, g_a).get
    println(s"a = Log_${g}_$g_a = $a")
    println(s"Verifying that $g^${log.get} = ${FastExpWithMod(p)(g, log.get)}")
    val g_ab = FastExpWithMod(p)(g_b, a)
    println(s"g^ab = $g_ab")
    val g_ab_inv = ModInverse(g_ab, p)
    println(s"g^(-ab) = $g_ab_inv")
    println(s"m = ${g_ab_inv * m_g_ab % p}")
  }

  private def ElGamalBob(p: Int, g: Int, g_a: Int, b: Int, m : Int){
    val g_ab = FastExpWithMod(p)(g_a, b)
    println(s"Let p = $p , g = $g,  b = $b, m = $m ")
    println(s"g^b = ${FastExpWithMod(p)(g,b)}")
    println(s"m*(g^(ab)) = ${(m * g_ab) % p}")
  }
}

private trait Person
private case object Alice extends Person
private case object Bob extends Person
private case object Eve extends Person
