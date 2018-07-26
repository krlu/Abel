package org.bu.metcs789.algebraicStructures.fields

class Complex(real: Double, imaginary: Double) extends Field[(Double, Double)] {
  override val zero: (Double, Double) = (0, 0)
  override val one: (Double, Double) = (1, 0)

  override def inverse(a: (Double, Double)): (Double, Double) = (-a._1, -a._2)
  override def add(a: (Double, Double), b: (Double, Double)): (Double, Double) = (a._1 + b._1, a._2 + b._2)
  override def sub(a: (Double, Double), b: (Double, Double)): (Double, Double) = (a._1 + b._1, a._2 + b._2)
  override def mult(a: (Double, Double), b: (Double, Double)): (Double, Double) = (a._1*b._1 - a._2*b._2, a._1*b._2 + a._2*b._1)
  override def div(a: (Double, Double), b: (Double, Double)): (Double, Double) = {
    val (x2, i2) = b
    val denominator = x2 * x2 + i2 * i2
    val (numerator1, numerator2) = mult(a, (x2, -i2))
    (numerator1/denominator, numerator2/denominator)
  }
  override def eq(a: (Double, Double), b: (Double, Double)): Boolean = a._1 == b._1 && a._2 == b._2
}
