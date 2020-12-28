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
}
