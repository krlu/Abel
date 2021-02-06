package org.bu

package object abel {

  def choose[A](it: Iterator[A]): A =
    it.zip(Iterator.iterate(1)(_ + 1)).reduceLeft((row, col) =>
      if (util.Random.nextInt(col._2) == 0) col else row
    )._1

  def combinationList[T](ls:List[List[T]]):List[List[T]] = ls match {
    case Nil => Nil::Nil
    case head :: tail => val rec = combinationList[T](tail)
      rec.flatMap((r: List[T]) => head.map((t: T) => t::r))
  }

  def removeLeadingZeroes(s: String): String = {
    val sb = new StringBuilder(s)
    while ( {
      sb.nonEmpty && sb.charAt(0) == '0'
    }) sb.deleteCharAt(0)
    sb.toString
  }

  def removeTrailingZeroes(s: String): String = {
    val sb = new StringBuilder(s)
    while ( {
      sb.nonEmpty && sb.charAt(sb.length - 1) == '0'
    }) sb.setLength(sb.length - 1)
    sb.toString
  }
  
}
