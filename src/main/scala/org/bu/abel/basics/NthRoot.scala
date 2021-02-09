package org.bu.abel.basics

import org.bu.abel.algops.rings.IntegerModN


protected class NthRoot(modulus: Long) extends ((Long, Long) => Seq[Long]){
  val elements = PrimeUtil.relPrimesLessThanN(modulus)
  val zModN = IntegerModN(modulus)
  override def apply(v1: Long, root: Long): Seq[Long] = {
    elements.filter(x => {
       zModN.pow(x, root) == v1
    })
  }
}
object NthRoot{
  def apply(modulus: Long): NthRoot = new NthRoot(modulus)
}
