package org.bu

package object metcs789 {
  def choose[A](it: Iterator[A]): A =
    it.zip(Iterator.iterate(1)(_ + 1)).reduceLeft((row, col) =>
      if (util.Random.nextInt(col._2) == 0) col else row
    )._1

  def combinationList[T](ls:List[List[T]]):List[List[T]] = ls match {
    case Nil => Nil::Nil
    case head :: tail => val rec = combinationList[T](tail)
      rec.flatMap((r: List[T]) => head.map((t: T) => t::r))
  }
}
