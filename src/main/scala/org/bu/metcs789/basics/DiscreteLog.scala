package org.bu.metcs789.basics


/**Computes Discrete Log using Baby Step Giant step algorithm*/
protected class DiscreteLog(modulus: Long) extends ((Long, Long) => Option[Long]){
  require(modulus > 0)
  override def apply(base: Long, value: Long): Option[Long] = {
    require(base > 0 && value > 0)
    val m = Math.ceil(Math.sqrt(modulus)).toLong
    val powerOf: Map[Int, Long] = (0 to m.toInt).map(i => i -> FastExpWithMod(modulus)(base, i)).toMap
    val C = FastExpWithMod(modulus)(ModInverse(base, modulus), m) // (b ^-1)^m
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
