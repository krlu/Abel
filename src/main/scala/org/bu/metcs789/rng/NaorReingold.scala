package org.bu.metcs789.rng

import org.bu.metcs789._
import org.bu.metcs789.basics.{FastExpWithMod, PrimesLessThanN, RelPrimesLessThanN}

class NaorReingold(n: Int) extends (Int => Int){
  private val primes = PrimesLessThanN(Math.pow(2, n).toInt)
  val p = choose(primes.iterator)
  val q = choose(primes.iterator)
  val N = p*q
  val range = (1 to N.toInt).toList.map(_.toLong)
  val As = (1 to n.toInt).map{ i =>
    val ai0: Long = choose(range.iterator)
    val ai1: Long = choose(range.iterator)
    Seq(ai0, ai1)
  }
  val g = FastExpWithMod(N)(choose(RelPrimesLessThanN(N).iterator),2)

  override def apply(queryValue: Int): Int = {
    val binaryList = toBinaryWithPadding(n, queryValue)
    val sumAs = binaryList.indices.map{i => As(i)(binaryList(i))}.sum
    val gPowSumAs = FastExpWithMod(N)(g, sumAs)
    val beta2nBin = toBinaryWithPadding(2*n, gPowSumAs)
    val rBin = (1 to 2*n.toInt).map{ _ => choose(Seq(0,1).iterator)}
    (beta2nBin zip rBin).map{case(a,b) => a*b}.sum % 2
  }
  private def toBinaryWithPadding(bitLength: Int, value: Long): List[Int] ={
    val binStr = value.toBinaryString
    (List.fill(bitLength - binStr.length)(0).mkString + binStr).toList.map{b => b.toString.toInt}
  }
}

object NaorReingold{
  def apply(n: Int): NaorReingold = new NaorReingold(n)
}
