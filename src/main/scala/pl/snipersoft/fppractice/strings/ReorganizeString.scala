package pl.snipersoft.fppractice.strings

import scala.annotation.tailrec
import scala.collection.immutable.SortedMap

object ReorganizeString extends App {

  assert(reorganize("aaa").isEmpty)
  println(reorganize("aaabc"))
  println(reorganize("aaabbcdef"))

  /**
   * In output each two adjacent characters must be different.
   * If it's not possible returning None.
   */
  def reorganize(s: String): Option[String] = {
    implicit val tupleOrdering: Ordering[(Char, Int)] = Ordering.by[(Char, Int), Int](_._2)

    @tailrec
    def tailRec(todo: SortedMap[Char, Int], acc: String): Option[String] = {

      def readTheOftenestCharacter: Option[(Char, Int)] =
        if (acc.isEmpty) Some(todo.maxBy(_._2))
        else todo.toSeq.find(_._1 != acc.head)


      if (todo.isEmpty) Some(acc)
      else {
        val maxValue: Option[(Char, Int)] = readTheOftenestCharacter
        if (maxValue.isEmpty) None
        else {
          val mv = maxValue.get
          val newTodo = if (mv._2 == 1) todo.removed(mv._1) else todo + mv.copy(_2 = mv._2-1)
          val newAcc = mv._1.toString + acc
          tailRec(newTodo, newAcc)
        }
      }
    }

    val counts = s.groupBy(char => char).view.mapValues(_.length).toSeq
    val sortedCounts: SortedMap[Char, Int] = SortedMap.from(counts)
    tailRec(sortedCounts, "")
  }
}
