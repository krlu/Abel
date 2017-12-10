package org.bu.metcs789.factorization

import org.bu.metcs789.basics._

object PollardRho extends (Long => Option[Long]){
  override def apply(n: Long): Option[Long] = {
    def g(x: Long): Long = FastExpWithMod(n)(x, 2) + 1 % n
    var x: Long = 2
    var d: Long = 1
    var y: Long = 2
    while(d == 1){
      x = g(x)
      y = g(g(y))
      d = GCD(Math.abs(x-y),n)._1
    }
    if(d == n) None else Some(d)
  }
}
