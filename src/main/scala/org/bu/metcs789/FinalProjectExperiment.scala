package org.bu.metcs789

import org.bu.metcs789.basics._
import org.bu.metcs789.factorization.PollardRho

object FinalProjectExperiment{

  def main(args: Array[String]) {

    println("************************************ RSA Experiment******************************************")

    println("Encrypting Message as Alice")
    RSAAlice(4187, 3313, 2)
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
    println()

    println(s"Sending message as Bob: ")
    ElGamalBob(941, 141, 788, 142, 729)
    println()

    println("Breaking El Gamal as Eve")
    ElGamalEve(1559, 569, 1372, Alice)
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
    val p = PollardRho(n).get.toInt
    val q = n/p
    println(s"Eve finds p = $p and q = $q")
    RSADecrypt(n, g, y, (p-1)*(q-1), Eve)
  }

  private def ElGamalAlice(p: Int, g: Int, a: Int): Unit ={
    println(s"Let p = $p, g = $g, a = $a")
    println(s"Alice sends  p = $p, g = $g, g^a = ${FastExpWithMod(p)(g,a)}")
  }

  private def ElGamalEve(p: Int, g: Int, g_power: Int, sender: Person): Unit ={
    val varName = sender match {
      case Alice => "a"
      case Bob => "b"
      case Eve => throw new IllegalArgumentException("Cannot pass eve to this method!")
    }
    val log = DiscreteLog(1559)(569, 1372)
    println(s"$varName = Log_${g}_$g_power = ${DiscreteLog(p)(g, g_power)}")
    println(s"Verifying that $g^${log.get} = ${FastExpWithMod(p)(g, log.get)}")
  }

  private def ElGamalBob(p: Int, g: Int, g_a: Int, b : Int, m : Int){
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
