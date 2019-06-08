package org.bu.abel.algebraicStructures.rings.polynomials

class MultivariatePolynomial[T](coeffs: Seq[Seq[T]]) extends (Seq[T] => T){
  override def apply(v1: Seq[T]): T = ???
  def apply2(vars: T*): T = ???
}

object MultivariatePolynomial{
  def apply[T]: MultivariatePolynomial[T] = new MultivariatePolynomial(null)
}
