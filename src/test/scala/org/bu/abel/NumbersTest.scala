package org.bu.abel

import org.bu.abel.algebraicStructures.types.{C, Q}
import org.scalatest.{FlatSpec, Matchers}

class NumbersTest extends FlatSpec with Matchers{

  "A rational number" should "support arithmetic operations" in {
    for(_ <- 1 to 100) {
      val numer = (Math.random() * 100).toInt + 1
      val denom = (Math.random() * 100).toInt + 1
      val factor = (Math.random() * 100).toInt + 1
      val q1 = Q(numer, denom)
      val q2 = Q(numer * factor, denom * factor)
      assert(q1 * q2 == (q1 ^ 2))
      assert(q1 == q2)
      assert(q1/q2 == Q(1,1))
      assert(q1 - q2 == Q(0,1))
      assert(q1 + q2 == q1 * 2)
    }
  }

  "A complex number" should "support arithmetic operations" in {
    for(_ <- 1 to 10) {
      val real = (Math.random() * 100).toInt
      val im = (Math.random() * 100).toInt
      val q1 = C(real, im)
      val q2 = C(real, -im)
      assert(q1.conjugate == q2 && q2.conjugate == q1)
      assert((q1 * q2).im == 0)
      assert((q1 * q2).re == (real * real + im * im))
      val sq = q1 ^ 2
      assert(sq.re == q1.re * q1.re - q1.im * q1.im)
      assert(sq.im == q1.im * 2 * q1.re)
    }
    // test roots of unit
    val i = C.i
    for(exp <- 1 to 100){
      exp % 4 match {
        case e if e == 0 =>
          assert((i ^ e) == C.one)
        case e if e == 1 =>
          assert((i ^ e) == C.i)
        case e if e == 2 =>
          assert((i ^ e) == C(-1,0))
        case e if e == 3 =>
          assert((i ^ e) == C(0,-1))
      }
    }

    // test that e ^ (i*pi) = -1
    val iPi = C.i * Math.PI
    assert(iPi.exp.re == - 1 && Math.abs(iPi.exp.im) < 1E-15)

    // test that i ^ i = e ^ (- pi/2)
    assert((C.i ^ C.i) == (iPi * i/2).exp)

    // test that e ^ (i pi/2) == i
    val identity = (C.i * Math.PI/2.0).exp
    assert(Math.abs(identity.re) < 1E-16 && identity.im == 1)
  }
}
