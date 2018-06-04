package org.bu.metcs789.algebraicStructures.fields

import org.bu.metcs789.basics.GCD

class Rational extends Field[(Int, Int)]{
  override val zero: (Int, Int) = (0,1)
  override val one: (Int, Int) = (1,1)
  override def mult(a: (Int, Int), b: (Int, Int)): (Int, Int) = {
    val (numerator, denominator) = (a._1 * b._1, a._2 * b._2)
    val (gcd , _) = GCD(numerator, denominator)
    (numerator/gcd.toInt, denominator/gcd.toInt)
  }
  override def div(a: (Int, Int), b: (Int, Int)): (Int, Int) = mult(a, (b._2, b._1))
  override def add(a: (Int, Int), b: (Int, Int)): (Int, Int) = {
    val denominator = a._2 * b._2
    val numerator = a._1 * b._2 + b._1 * a._2
    val (gcd , _) = GCD(numerator, denominator)
    (numerator/gcd.toInt, denominator/gcd.toInt)
  }
  override def sub(a: (Int, Int), b: (Int, Int)): (Int, Int) = add(a, (-b._1, b._2))
  override def eq(a: (Int, Int), b: (Int, Int)): Boolean = a._1 == b._1 && a._2 == b._2

  override def inverse(a: (Int, Int)): (Int, Int) = (-a._1, a._2)
}
