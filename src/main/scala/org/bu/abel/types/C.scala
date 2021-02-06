package org.bu.abel.types

/**
  * Class representing a C number, denoted by C - symbol for set of C numbers
  * @param re - real part
  * @param im - imaginary coefficient
  */
class C(val re: Double, val im: Double) {

  lazy val conjugate: C = C(re, -im)
  lazy val abs: Double = math.sqrt(re * re + im * im)
  lazy val log: C = C(math.log(abs), math.atan2(im, re))
  lazy val exp: C = {
    val expreal = math.exp(re)
    C(expreal * math.cos(im), expreal * math.sin(im))
  }

  val conjugateProd: Double = re*re + im*im
  def +(other: C): C = C(re + other.re, im + other.im)
  def -(other: C): C = C(re - other.re, im - other.im)
  def *(x: Double):  C = C(re * x, im * x)
  def *(other: C): C = C(re * other.re - im * other.im, re * other.im + im * other.re)
  def /(x: Double):  C = C(re / x, im / x)
  def / (other: C): C = {
    val denominator = conjugateProd
    val numerator = this * other.conjugate
    numerator/denominator
  }
  def ^(e: Int): C = {
    require(e >= 0)
    if(e == 0) C(1,0)
    else if(e == 1) this
    else if(e % 2 == 0) (this * this) ^ (e/2)
    else this * ((this * this) ^ ((e - 1)/2))
  }

  def ^(c: C): C = (this * C.i * Math.PI/2).exp

  override def equals(other: Any): Boolean = other match {
    case c: C => this.re == c.re && this.im == c.im
    case _ => false
  }
  def != (other: C): Boolean = !this.equals(other)
  def == (other: C): Boolean = this.equals(other)
  override def toString: String = s"$re + ${im}i"
}

object C{
  def apply(re: Double, im: Double): C = new C(re, im)
  def zero: C = C(0,0)
  def one: C = C(1,0)
  def i: C = C(0,1)
}
