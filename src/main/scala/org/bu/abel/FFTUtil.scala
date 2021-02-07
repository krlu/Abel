package org.bu.abel

import org.bu.abel.basics.LargeNumber
import org.bu.abel.types.C
object FFTUtil{

  def round(value: C, decimal: Int): C = {
    val re: LargeNumber = value.re
    val im: LargeNumber = value.im
    C.apply(round(re, decimal), round(im, decimal))
  }

  def round(value: LargeNumber, decimal: Int): LargeNumber = {
    val factor = Math.pow(10, decimal).toInt
    LargeNumber.round(value * factor)/ factor
  }

  def toComplex(vector: Array[LargeNumber]): Array[C] = vector.map(e =>C(e, LargeNumber(0,0)))

  def intToLargeNum(vector: Array[Int], precision: Int): Array[LargeNumber] =
    vector.map(e => if(precision > 0) LargeNumber(e, precision) else LargeNumber(e))

  def doubleToLargeNum(vector: Array[Double], precision: Int): Array[LargeNumber] =
    vector.map(e => if(precision > 0) LargeNumber(e, precision) else LargeNumber(e))

  def complexToLargeNum(vector: Array[C], precision: Int): Array[LargeNumber] =
    vector.map{ e => if(precision > 0) round(e.re, precision)  else e.re}

  def intToLargeNum(vector: Array[Int]): Array[LargeNumber] = intToLargeNum(vector, 0)

  def doubleToInteger(vector: Array[Double]): Array[Int] = vector.map(e =>(e + 0.5).toInt)
}
