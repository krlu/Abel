package org.bu.metcs789

import org.bu.metcs789.Basics.{FastExp, ModInverse}


/**Computes Discrete Log using Baby Step Giant step algorithm*/
protected class DiscreteLog(modulus: Long) extends ((Long, Long) => Option[Long]){
  require(modulus > 0)
  override def apply(base: Long, value: Long): Option[Long] = {
    require(base > 0 && value > 0)
    val m = Math.ceil(Math.sqrt(modulus)).toLong
    val powerOf: Map[Int, Long] = (0 to m.toInt).map(i => i -> FastExp(base, i).toLong % modulus).toMap
    val C = FastExp(ModInverse(base, modulus), m).toLong // (b ^-1)^m
    var testValue = value
    for(i <- 0 to m.toInt) {
      for(j <- powerOf.keys){
        if(powerOf(j) == testValue)
          return Some((i*m + j) % modulus)
      }
      testValue = (testValue * C) % modulus
    }
    None
  }
}

object DiscreteLog{
  def apply(modulus: Long): DiscreteLog = new DiscreteLog(modulus)
}
