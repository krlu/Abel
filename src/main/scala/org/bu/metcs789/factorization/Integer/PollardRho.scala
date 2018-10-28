package org.bu.metcs789.factorization.Integer

import org.bu.metcs789.basics._

object PollardRho extends (Long => Long){

  /**
    * This method applies PollardRho twice, once using +1 offset and once using -1+n offset
    * Whichever one returns a factor greater than 1 is the desired factor
    * @param n - integer to be factored
    * @return factor of n
    */
  override def apply(n: Long): Long = {
    val f1 = helper(n, 1)
    val f2 = helper(n , offset =  -1 + n)
    if(f1 == 1) f2 else f1
  }

  def helper(n: Long, offset: Long): Long = {
    if(n%2 == 0) return 2
    def g(x: Long): Long = (FastExpWithMod(n)(x, 2) + offset) % n
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
