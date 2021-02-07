package org.bu.abel

import org.bu.abel.types.C

class ScalaFFT {
  val precision = 0
  def fft(a: Array[C], level: Int): Array[C] = {
    val n = a.length
    if(a.length == 1)
      return a
    else {
      val e = C(Math.E, 0.0)
      val exponent = C.i * 2*Math.PI/n
      val principal = (e ^ exponent).conjugate
    }
    null
  }
}
