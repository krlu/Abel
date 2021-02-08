package org.bu.abel.encryption

import org.bu.abel.algops.rings.IntegerRing
import org.bu.abel.basics.IsPrime

/**
  * @param g - generator
  * @param p - prime modulus
  */
class DiffieHellman(g: Long, p: Long) extends ((DHUser, DHUser) => Unit){
  val Z: IntegerRing = IntegerRing()
  require(IsPrime(p) && g < p && p > 0 && g > 0)
  /**
    * @param alice - User to send a key
    * @param bob - User to receive a Key
    */
  override def apply(alice: DHUser, bob: DHUser): Unit = {
    bob.intermediateKeys = bob.intermediateKeys :+ Z.pow(g, alice.privateKey) % p
    alice.intermediateKeys = alice.intermediateKeys :+ Z.pow(g, bob.privateKey) % p
    alice.sharedKey = Z.pow(alice.intermediateKeys.last, alice.privateKey) % p
    bob.sharedKey = Z.pow(bob.intermediateKeys.last, bob.privateKey) % p
  }
}

case class DHUser(var privateKey: Long, var sharedKey: Long, var intermediateKeys: Seq[Long])

object DiffieHellman{
  def apply(g: Long, p: Long): DiffieHellman = new DiffieHellman(g, p)
}
