package org.bu.metcs789.factorization.polynomial

import org.bu.metcs789.algebraicStructures.rings.polynomials.RealPolynomial

/**
  * Performs multiple instances of newtons method with multiple guesses, expanding outwards from the original guess
  * @param initialGuess -  initial guess for root of polynomial
  * @param maxIterations - number of iterations of newtons method before stopping and instance of newtons method
  * @param maxGuesses - maximum number of initial guesses before stopping all instances of newtons method
  */
protected class NewtonsMethod(initialGuess: Double, maxIterations: Int, maxGuesses: Int) extends PolynomialFactorizationAlgo {
  private var guess = initialGuess
  private var radius = 1
  override def apply(p: RealPolynomial): Seq[RealPolynomial] = {
    for( numGuesses <- 0 until maxGuesses) {
      val factors = newtonsHelper(p)
      if(factors.size > 1){
        return factors.flatMap(NewtonsMethod(initialGuess = initialGuess)(_))
      }else {
        guess = if(numGuesses%2 == 0 ) initialGuess - radius else initialGuess + radius
        radius += 1
      }
    }
    Seq(p)
  }
  /**
    * Uses newtons method to find of Polynomial
    * Is much faster than Kronecker's method, but will break if Polynomial does not have integer roots.
    * @param p - Polynomial to be Factored
    * @return Seq[Polynomial] Seq of factors of polynomial (if found)
    */
  def newtonsHelper(p: RealPolynomial): Seq[RealPolynomial] ={
    var root = guess
    if(p.degree <= 2)
      return Kronecker(p)
    var numIterations = 0
    while(p(root) != 0 && numIterations < maxIterations){
      root = root - (p(root)/p.derivative(root))
      numIterations += 1
    }
    if(p(root) != 0) Seq(p)
    else{
      val roundedRoot = if(root < 0) - Math.round(Math.abs(root)) else Math.round(root)
      val factor = RealPolynomial(-Math.round(roundedRoot),1)
      val a = newtonsHelper((p/factor)._1)
      val b = newtonsHelper(factor)
      a ++ b
    }
  }
}

object NewtonsMethod{
  def apply(initialGuess: Double = 1.0, maxIterations: Int = 10000, maxGuesses: Int = 10): NewtonsMethod =
    new NewtonsMethod(initialGuess, maxIterations, maxGuesses)
}
