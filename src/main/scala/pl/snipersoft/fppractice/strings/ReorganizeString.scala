package pl.snipersoft.fppractice.strings

import scala.annotation.tailrec

object ReorganizeString extends App {

  assert(reorganize("aaa").isEmpty)
  println(reorganize("aaabc"))

  /**
   * In output each two adjacent characters must be different.
   * If it's not possible returning None.
   */
  def reorganize(s: String): Option[String] = {

    @tailrec
    def tailRec(todo: Map[Char, Int], acc: String): Option[String] = {
      def readTheOftenestCharacter: Option[(Char, Int)] =
        if (acc.isEmpty) Some(todo.maxBy(_._2))
        else todo.toSeq.filter(_._1 != acc.head).sortWith((o1, o2) => o1._2 > o2._2).headOption


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

    val counts: Map[Char, Int] = s.groupBy(char => char).view.mapValues(_.length).toMap
    tailRec(counts, "")
  }
}
