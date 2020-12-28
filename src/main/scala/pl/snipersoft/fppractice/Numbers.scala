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

  def decompose(n: Int): List[Int] = {
    @tailrec
    def helper(check: Int, result: List[Int]): List[Int] = {
      if (check > n) return result

      if (n % check == 0) helper(check + 1, result ++ Seq(check))
      else helper(check + 1, result)
    }

    if (n<0) return decompose(-n)
    helper(1, Nil)
  }
}
