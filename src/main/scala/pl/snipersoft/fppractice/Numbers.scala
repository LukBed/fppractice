package pl.snipersoft.fppractice

import scala.annotation.tailrec

object Numbers {
  def isPrime(n: Int): Boolean = {
    @tailrec
    def helper(check: Int): Boolean =
      check >= n/2+1 || (n % check != 0 && helper(check + 1))

    if (n >= 2) {
      helper(2)
    } else
      false
  }

  @tailrec
  def decompose(n: Int): List[Int] = {
    @tailrec
    def helper(check: Int, result: List[Int]): List[Int] = {
      if (check >= n/2 + 1) return result :+ n

      if (n % check == 0) helper(check + 1, result :+ check)
      else helper(check + 1, result)
    }

    n match {
      case 0 => Nil
      case n if n<0 => decompose(-n)
      case _ => helper(1, Nil)
    }
  }
}
