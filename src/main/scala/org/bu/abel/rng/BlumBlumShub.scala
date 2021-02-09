package org.bu.abel.rng

import org.bu.abel._
import org.bu.abel.algops.rings.IntegerModN
import org.bu.abel.basics.PrimeUtil

class BlumBlumShub(p: Long, q: Long){
  require(p%4 == 3 && q%4 == 3)
  val n: Long = p*q
  val zModN = IntegerModN(n)
  private var s0 = choose(PrimeUtil.relPrimesLessThanN(n).iterator)
  private var mostRecentS: Option[Long] = None
  def generateBit: Long = {
    mostRecentS match {
      case None =>
        mostRecentS = Some(s0)
        s0 % 2
      case Some(number) =>
        mostRecentS = Some(zModN.pow(number, 2))
        number % 2
    }
  }
  def resetSeed(): Unit ={
    s0 = choose(PrimeUtil.relPrimesLessThanN(n).iterator)
    mostRecentS = None
  }
}
object BlumBlumShub{
  def apply(p: Long, q: Long): BlumBlumShub = new BlumBlumShub(p, q)
}
