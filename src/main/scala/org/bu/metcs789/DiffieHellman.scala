package org.bu.metcs789

import org.bu.metcs789.Basics.{FastExp, IsPrime}

class DiffieHellman(g: Long, p: Long) extends ((User, User) => Unit){
  require(IsPrime(p) && g < p && p > 0 && g > 0)
  override def apply(alice: User, bob: User): Unit = {
    bob.intermediateKeys = bob.intermediateKeys :+ FastExp(g, alice.privateKey).toLong % p
    alice.intermediateKeys = alice.intermediateKeys :+ FastExp(g, bob.privateKey).toLong % p
    alice.sharedKey = FastExp(alice.intermediateKeys.last, alice.privateKey).toLong % p
    bob.sharedKey = FastExp(bob.intermediateKeys.last, bob.privateKey).toLong % p
  }
}

case class User(var privateKey: Long, var sharedKey: Long, var intermediateKeys: Seq[Long])

object DiffieHellman{
  def apply(g: Long, p: Long): DiffieHellman = new DiffieHellman(g, p)
}
