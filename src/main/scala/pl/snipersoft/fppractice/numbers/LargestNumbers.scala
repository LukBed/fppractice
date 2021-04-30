package pl.snipersoft.fppractice.numbers

import scala.annotation.tailrec

object LargestNumbers {
  def prepare(numbers: Seq[Int]): Option[String] =
    if (numbers.exists(_ < 0)) None
    else if (numbers.isEmpty) Some("0")
    else Some(prepareForCorrectList(numbers))

  private def prepareForCorrectList(numbers: Seq[Int]): String = numbers.sortWith(isAStronger).mkString("")

  def isAStronger(a: Int, b: Int): Boolean = {

    @tailrec
    def tailRec(as: List[Int], bs: List[Int]): Boolean = {
      if (as.isEmpty && bs.isEmpty) false
      else if (as.isEmpty) bs.head == 0
      else if (bs.isEmpty) as.head != 0
      else if (as.head == bs.head) tailRec(as.tail, bs.tail)
      else as.head>bs.head
    }

    if (a == 0) true
    else if (b == 0) false
    else tailRec(toDigits(a), toDigits(b))
  }

  private def toDigits(x: Int): List[Int] = {
    @tailrec
    def tailRec(todo: Int, acc: List[Int]): List[Int] = {
      if (todo == 0) acc
      else tailRec(todo/10, todo%10 :: acc)
    }

    tailRec(x, List.empty)
  }
}