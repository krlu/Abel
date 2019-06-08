package org.bu.abel.algebraicStructures.rings

import org.apache.commons.math3.linear.{MatrixUtils, RealMatrix}

class MatrixRing(dim: Int) extends Ring[RealMatrix]{

  override val one: RealMatrix = MatrixUtils.createRealIdentityMatrix(dim)

  override def mult(a: RealMatrix, b: RealMatrix): RealMatrix = a.multiply(b)

  override val zero: RealMatrix = MatrixUtils.createRealIdentityMatrix(dim)

  override def inverse(a: RealMatrix): RealMatrix = MatrixUtils.inverse(a)

  override def add(a: RealMatrix, b: RealMatrix): RealMatrix = a.add(b)

  override def sub(a: RealMatrix, b: RealMatrix): RealMatrix = a.subtract(b)

  override def eq(a: RealMatrix, b: RealMatrix): Boolean = a == b
}

object MatrixRing {
  def apply(dim: Int): MatrixRing = new MatrixRing(dim)
}
