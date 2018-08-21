package org.bu.metcs789.factorization.polynomial

import org.bu.metcs789.algebraicStructures.rings.polynomials.RealPolynomial

/**
  * @param initialGuess -  initial guess for root of polynomial
  * @param maxIterations - number of iterations of newtons method before stopping
  */
protected class NewtonsMethod(initialGuess: Double, maxIterations: Int) extends PolynomialFactorizationAlgo {
  override def apply(p: RealPolynomial): Seq[RealPolynomial] = newtonsHelper(p)

  /**
    * Uses newtons method to find of Polynomial
    * Is much faster than Kronecker's method, but will break if Polynomial does not have integer roots.
    * @param p - Polynomial to be Factored
    * @return Seq[Polynomial] Seq of factors of polynomial (if found)
    */
  def newtonsHelper(p: RealPolynomial): Seq[RealPolynomial] ={
    var root = -1.0
    if(p.degree <= 2)
      return Kronecker(p)
    var numIterations = 0
    while(p(root) != 0 && numIterations < maxIterations){
      root = root - (p(root)/p.derivative(root))
      numIterations += 1
    }
    if(p(root) != 0) Seq(p)
    else{
      val factor = RealPolynomial(-root,1)
      val a = newtonsHelper((p/factor)._1)
      val b = newtonsHelper(factor)
      a ++ b
    }
  }
}

object NewtonsMethod{
  def apply(initialGuess: Double = -1.0, maxIterations: Int = 100): NewtonsMethod =
    new NewtonsMethod(initialGuess, maxIterations)
}
