package pl.snipersoft.fppractice.strings

import scala.annotation.tailrec
import scala.language.postfixOps

object StringUtils {
  implicit class RichString(s: String) {
    def countCharacters: Map[Char, Int] = {
      @tailrec
      def helper(value: String, accumulator: Map[Char, Int]): Map[Char, Int] = {
        if (value.length == 0) accumulator
        else {
          val firstChar = value.charAt(0)
          val countOfFirstChar = accumulator.getOrElse(firstChar, 0) + 1
          val newAccumulator = accumulator + (firstChar -> countOfFirstChar)
          if (value.length == 1) {
            newAccumulator
          } else {
            helper(value.substring(1), newAccumulator)
          }
        }
      }

      helper(s, Map.empty)
    }
  }
}
