package org.bu.metcs789.factorization

import org.bu.metcs789.basics._

object PollardRho extends (Long => Long){
  override def apply(n: Long): Long = {
    if(n%2 == 0) return 2
    def g(x: Long): Long = (FastExpWithMod(n)(x, 2) + 1) % n
    var x: Long = 2
    var y: Long = 2
    var d: Long = 1
    while(d == 1){
      x = g(x)
      y = g(g(y))
      d = GCD(Math.abs(x-y),n)._1
    }
    if(d == n) 1 else d
  }
}
