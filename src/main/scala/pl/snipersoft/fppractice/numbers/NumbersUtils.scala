package pl.snipersoft.fppractice.numbers

import scala.annotation.tailrec
import scala.language.postfixOps

object NumbersUtils {

  implicit class RichInt(n: Int) {

    def isPrime: Boolean = {
      @tailrec
      def helper(check: Int): Boolean =
        check >= n / 2 + 1 || (n % check != 0 && helper(check + 1))

      if (n >= 2) {
        helper(2)
      } else
        false
    }

    def decompose: List[Int] = {
      @tailrec
      def helper(check: Int, result: List[Int]): List[Int] = {
        if (check >= n / 2 + 1) return result :+ n

        if (n % check == 0) helper(check + 1, result :+ check)
        else helper(check + 1, result)
      }

      n match {
        case 0 => Nil
        case n if n < 0 => -n decompose
        case _ => helper(1, Nil)
      }
    }
  }

}
