package org.bu.abel

import org.bu.abel.types.{C, LargeNumber}
import org.bu.abel.types.polynomials.RealPolynomial

object LargeFFT {

  // Note: performance is slow due to BigFloatLibrary
  def main(args: Array[String]): Unit = {
    val p1 = RealPolynomial(1,1,1)
    for(exp <- 1000 to 1000) {
      val t1 = System.currentTimeMillis()
      println((p1 ^ exp).coefficients.toList(exp/2))
      val t2 = System.currentTimeMillis()
      println(t2 - t1)
    }
  }

  val precision = 0

  def multiply(a: Array[LargeNumber], b: Array[LargeNumber]): Array[LargeNumber] = {
    val n = a.length + b.length - 1
    var exp = 1
    while(Math.pow(2, exp) < n){
      exp+=1
    }
    val length = Math.pow(2,exp).toInt
    val aPadded = pad(a, length)
    val bPadded = pad(b, length)

    val aPrime = transform(aPadded)
    val bPrime = transform(bPadded)

    val cPrime = (0 until length).map(i => aPrime(i) * bPrime(i)).toArray
    val c = inverse(cPrime)
    if(a.toList.forall(_.isInteger) && b.toList.forall(_.isInteger)) c.take(n).map(LargeNumber.round)
    else
      c.take(n)
  }


  private def pad(vector: Array[LargeNumber], size: Int) =
    vector ++ Array.fill(size - vector.length)(LargeNumber(0.0))

  def fft(a: Array[C]): Array[C] = {
    val n = a.length
    if(a.length == 1) a
    else {
      val e = C(Math.E, 0.0)
      val exponent = C.i * 2*Math.PI/n
      var principal = (e ^ exponent).conjugate
      principal = FFTUtil.round(principal, 10)
      var omega = C(1, 0)
      val halfUp = Math.ceil(n/2.0).toInt
      val halfDown = n/2

      val a0: Array[C] = Array.fill(halfUp)(C(0,0))
      val a1: Array[C] = Array.fill(halfDown)(C(0,0))
      for(i <- 0 until halfUp){
        a0(i) = a(i*2)
      }
      for(i <- 0 until halfDown){
        a1(i) = a(i*2 + 1)
      }
      val y0 = fft(a0)
      val y1 = fft(a1)
      val y: Array[C] = Array.fill(n)(null)
      for(k <- 0 until halfDown){
        omega = FFTUtil.round(omega, 5)
        y(k) = y0(k) + (omega * y1(k))
        y(k + halfDown) = y0(k) - (omega * y1(k))
        omega = omega * principal
      }
      y
    }
  }

  private def transform(vector: Array[C], inverse: Boolean): Array[C] = {
    val transformedVector = if(inverse) fft(vector.map(e => e.conjugate)) else fft(vector)
    if(inverse)
      transformedVector.map(e =>e.conjugate/vector.length)
    else transformedVector
  }

  def transform(vector: Array[LargeNumber]): Array[C] =
    transform(FFTUtil.toComplex(vector), inverse = false)

  def inverse(vector: Array[C]): Array[LargeNumber] =
    FFTUtil.complexToLargeNum(transform(vector, inverse = true), precision)
}
