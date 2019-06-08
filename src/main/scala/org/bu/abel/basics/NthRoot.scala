package org.bu.abel.basics


protected class NthRoot(modulus: Long) extends ((Long, Long) => Seq[Long]){
  val elements = RelPrimesLessThanN(modulus)
  override def apply(v1: Long, root: Long): Seq[Long] = {
    elements.filter(x => {
       FastExpWithMod(modulus)(x, root).toLong == v1
    })
  }
}
object NthRoot{
  def apply(modulus: Long): NthRoot = new NthRoot(modulus)
}
