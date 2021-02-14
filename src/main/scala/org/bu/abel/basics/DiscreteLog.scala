package org.bu.abel.basics

import org.bu.abel.algops.fields.IntegerModN


/**Computes Discrete Log using Baby Step Giant step algorithm*/
protected class DiscreteLog(modulus: Long) extends ((Long, Long) => Option[Long]){
  require(modulus > 0)
  val zModN: IntegerModN = IntegerModN(modulus)
  override def apply(base: Long, value: Long): Option[Long] = {
    require(base > 0 && value > 0)
    val m = Math.ceil(Math.sqrt(modulus)).toLong
    val powerOf: Map[Int, Long] = (0 to m.toInt).map(i => i -> zModN.pow(base, i)).toMap
    val C = zModN.pow(IntegerModN.modInverse(base, modulus), m) // (b ^-1)^m
    var y = value
    for(i <- 0 until m.toInt) {
      for(j <- powerOf.keys){
        if(powerOf(j) == y) {
          return Some((i * m + j) % modulus)
        }
      }
      y = (y * C) % modulus
    }
    None
  }
}

object DiscreteLog{
  def apply(modulus: Long): DiscreteLog = new DiscreteLog(modulus)
}

protected class AllDiscreteLogs(modulus: Long) extends ((Long, Long) => Seq[Long]){
  require(modulus > 0)
  val zModN: IntegerModN = IntegerModN(modulus)
  override def apply(base: Long, value: Long): Seq[Long] = {
    require(base > 0 && value > 0)
    var logs = Seq.empty[Long]
    val m = Math.ceil(Math.sqrt(modulus)).toLong
    val powerOf: Map[Int, Long] = (0 to m.toInt).map(i => i -> zModN.pow(base, i)).toMap
    val C = zModN.pow(IntegerModN.modInverse(base, modulus), m) // (b ^-1)^m
    var y = value
    for(i <- 0 until m.toInt) {
      for(j <- powerOf.keys){
        if(powerOf(j) == y) {
          logs = logs ++ Seq((i * m + j) % modulus)
        }
      }
      y = (y * C) % modulus
    }
    logs
  }
}

object AllDiscreteLogs{
  def apply(modulus: Long): AllDiscreteLogs = new AllDiscreteLogs(modulus)
}
