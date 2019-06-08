package org.bu.abel.rng

import org.bu.abel._
import org.bu.abel.basics.{FastExpWithMod, RelPrimesLessThanN}

class BlumBlumShub(p: Long, q: Long){
  require(p%4 == 3 && q%4 == 3)
  val n = p*q
  private var s0 = choose(RelPrimesLessThanN(n).iterator)
  private var mostRecentS: Option[Long] = None
  def generateBit: Long = {
    mostRecentS match {
      case None =>
        mostRecentS = Some(s0)
        s0 % 2
      case Some(number) =>
        mostRecentS = Some(FastExpWithMod(n)(number, 2))
        number % 2
    }
  }
  def resetSeed(): Unit ={
    s0 = choose(RelPrimesLessThanN(n).iterator)
    mostRecentS = None
  }
}
object BlumBlumShub{
  def apply(p: Long, q: Long): BlumBlumShub = new BlumBlumShub(p, q)
}
