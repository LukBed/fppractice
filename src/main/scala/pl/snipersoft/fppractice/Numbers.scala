package pl.snipersoft.fppractice

import scala.annotation.tailrec

object Numbers {
  def isPrime(n: Int): Boolean = {
    @tailrec
    def helper(n: Int, check: Int): Boolean = {
      if (check >= n) return true

      if (n % check == 0) {
        return false
      }

      helper(n, check + 1)
    }

    if (n >= 2) {
      helper(n, 2)
    } else
      false
  }
}
