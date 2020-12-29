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

        currentString.headOption match {
          case Some('(') => helper(currentString.tail, balance+1)
          case Some(')') => helper(currentString.tail, balance-1)
          case None => balance == 0
        }
      }

      helper(s, 0)
    }

    def justify(width: Int): String = {
      @tailrec
      def addWordsToLines(wordsToBeAdd: List[String], lines: List[Line]): List[Line] = {
        if (wordsToBeAdd.isEmpty) return lines

        val currentWord = wordsToBeAdd.head
        val currentLine = lines.last

        if (currentLine.length + currentWord.length < width) {
          val newLines = lines.slice(0, lines.length-1) :+ Line(currentLine.words :+ currentWord)
          addWordsToLines(wordsToBeAdd.tail, newLines)
        } else {
          val newLines = lines :+ Line(List(currentWord))
          addWordsToLines(wordsToBeAdd.tail, newLines)
        }
      }

      case class Line(words: List[String]) {
        lazy val length: Int = words match {
          case Nil => 0
          case List(x) => x.length
          case _ => words.map(_.length).sum + words.length - 1
        }

        def value(): String = {
          words.mkString(" ")
        }
      }

      val words = s.split(" ").toList
      val lines = addWordsToLines(words, List(Line(Nil)))
      lines.map(_.value()).mkString("\n")
    }
  }

  def generateAllValidParentheses(n: Int): Set[String] = {
    @tailrec
    def helper(current: Int, accumulator: Set[String]): Set[String] = {
      if (current == n) return accumulator

      val newAcc = accumulator.flatMap(v => Set(s"($v)", s"()$v", s"$v()"))
      helper(current+1, newAcc)
    }

    n match {
      case 0 => Set.empty
      case _ => helper(1, Set("()"))
    }
  }
}
