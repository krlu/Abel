package org.bu.metcs789.Basics


protected class NthRoot(modulus: Int) extends ((Int, Int) => Seq[Int]){
  val elements = RelPrimesLessThanN(modulus)
  override def apply(v1: Int, root: Int): Seq[Int] = {
    elements.filter(x => {
       FastExpWithMod(modulus)(x, root) == v1
    }).map(_.toInt)
  }
}
object NthRoot{
  def apply(modulus: Int): NthRoot = new NthRoot(modulus)
}
