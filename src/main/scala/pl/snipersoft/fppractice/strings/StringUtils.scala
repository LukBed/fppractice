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

    def isAnagramOf(s2: String): Boolean = {
      if (s.length != s2.length) return false

      @tailrec
      def helper(currentIndex: Int): Boolean = {
        if (currentIndex >= s.length) return true

        if (s.charAt(currentIndex) == s2.charAt(s2.length - currentIndex - 1)) {
          helper(currentIndex + 1)
        } else
          false
      }

      helper(0)
    }
  }
}

object T extends App {
  import StringUtils._
  println("AbcD".isAnagramOf("dcbA"))
}
