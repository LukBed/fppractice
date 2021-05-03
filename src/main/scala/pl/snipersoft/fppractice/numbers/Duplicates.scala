package pl.snipersoft.fppractice.numbers

import scala.annotation.tailrec

object Duplicates {
  /**
   * Input is list of integers. All numbers in the list appear exactly twice, except one number.
   * Returning this one number.
   */
  def findNotDuplicatedNumber(list: List[Int]): Int = {
    @tailrec
    def tailRec(todo: List[Int], uniques: Set[Int]): Int = {
      if (todo.isEmpty) uniques.head
      else {
        val head = todo.head
        if (uniques.contains(head)) tailRec(todo.tail, uniques - head)
        else tailRec(todo.tail, uniques + head)
      }
    }

    tailRec(list, Set())
  }

}
