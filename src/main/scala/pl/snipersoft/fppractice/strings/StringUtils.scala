package pl.snipersoft.fppractice.strings

import scala.annotation.tailrec
import scala.language.postfixOps

object StringUtils {

  implicit class RichString(s: String) {
    def countCharacters: Map[Char, Int] = {
      @tailrec
      def helper(value: String, accumulator: Map[Char, Int]): Map[Char, Int] = {
        if (value.headOption.isEmpty) return accumulator

        val firstChar = value.head
        val countOfFirstChar = accumulator.getOrElse(firstChar, 0) + 1
        val newAccumulator = accumulator + (firstChar -> countOfFirstChar)

        value.tail match {
          case "" => newAccumulator
          case _ => helper(value.substring(1), newAccumulator)
        }
      }

      helper(s, Map.empty)
    }

    def isAnagramOf(s2: String): Boolean = s.sorted == s2.sorted

    def hasValidParentheses: Boolean = {
      if (s.chars().anyMatch(ch => '(' != ch && ')' != ch)) return false

      @tailrec
      def helper(currentString: String, balance: Int): Boolean = {
        if (balance<0) return false
        if (currentString.isEmpty && balance == 0) return true
        if (currentString.isEmpty && balance != 0) return false

        val rest = currentString.substring(1)

        currentString.charAt(0) match {
          case '(' => helper(rest, balance+1)
          case ')' => helper(rest, balance-1)
        }
      }

      helper(s, 0)
    }
  }
}
