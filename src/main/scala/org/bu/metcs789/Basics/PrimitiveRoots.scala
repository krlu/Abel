package org.bu.metcs789.Basics

/**
  * Finds all G < P such that for all A co-prime to P we have:
  *   exists K such that Math.pow(G,K) = A
  */
object PrimitiveRoots extends (Int => Seq[Int]) {
  override def apply(p: Int): Seq[Int] = {
    require(p > 0)
    var pRoots = Seq.empty[Int]
    val relPRimes = RelPrimesLessThanN(p)
    for(n <- 1 to p){
      if(GCD(n,p)._1 == 1){
        var calculatedVals = Seq(1)
        var currentVal = 1
        while(calculatedVals.count(_ == currentVal) < 2){
          currentVal = (currentVal * n)%p
          calculatedVals = calculatedVals :+ currentVal
        }
        if(calculatedVals.toSet == relPRimes.toSet)
          pRoots = pRoots :+ n
      }
    }
    pRoots
  }
}
