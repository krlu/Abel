package org.bu.abel.rng

import org.bu.abel._
import org.bu.abel.algops.rings.IntegerModN
import org.bu.abel.basics.PrimeUtil


class NaorReingold(n: Int) extends (Int => Int){
  private val primes = PrimeUtil.primesLessThanN(Math.pow(2, n).toInt)
  val p: Long = choose(primes.iterator)
  val q: Long = choose(primes.iterator)
  val N: Long = p*q
  val range: Seq[Long] = (1 to N.toInt).toList.map(_.toLong)
  val As: Seq[Seq[Long]] = (1 to n.toInt).map{ _ =>
    val ai0: Long = choose(range.iterator)
    val ai1: Long = choose(range.iterator)
    Seq(ai0, ai1)
  }
  val zModN: IntegerModN = IntegerModN(N)
  val g: Long = zModN.pow(choose(PrimeUtil.relPrimesLessThanN(N).iterator),2)

  override def apply(queryValue: Int): Int = {
    val binaryList = toBinaryWithPadding(n, queryValue)
    val sumAs = binaryList.indices.map{i => As(i)(binaryList(i))}.sum
    val gPowSumAs = zModN.pow(g, sumAs)
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
