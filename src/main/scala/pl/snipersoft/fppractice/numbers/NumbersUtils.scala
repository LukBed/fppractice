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

    def reverse: Option[Int] =
      if (n == Int.MinValue) None
      else if (n >= 0) n.toString.reverse.toIntOption
      else (-n).reverse.map(-_)
  }

  /**
   * Creating Option[Int] from String.
   * Ignoring leading spaces.
   * Reading the sign character if present.
   * Reading all the digits until the end of the string or until a non-digit character.
   * Return the number formed from those digits.
   * If the number exceeds the Int range returning None.
   */
  def parseInt(input: String): Option[Int] = {
    case class Prepared(toParse: String, isNegative: Boolean)
    def prepare = {
      val noLeadSpaces = input.stripLeading()
      val noSign = if (noLeadSpaces.startsWith("+") || noLeadSpaces.startsWith("-")) noLeadSpaces.substring(1) else noLeadSpaces
      val isNegative = noLeadSpaces.startsWith("-")
      val lastChar: Option[Char] = noSign.toCharArray.find(character => character < '0' || character > '9')
      val lastIndex: Int = lastChar.map(noSign.indexOf(_)).getOrElse(noSign.length)
      val toParse = noSign.substring(0, lastIndex)
      Prepared(toParse, isNegative)
    }

    def toDigits(s: String): List[Int] = List.from(s.toCharArray).map(character => character-'0')

    def executeParsing(toParse: String): Option[Int] = {
      val digits: List[Int] = toDigits(toParse)
      val maxDigits: List[Int] = toDigits(Int.MaxValue.toString)

      @tailrec
      def tailRec(todoCheck: List[Int], todoInt: List[Int], acc: Int, noneIfNextIsGreater: Boolean): Option[Int] = {
        if (todoCheck.isEmpty) Some(acc)
        else if (todoCheck.size<todoInt.size) tailRec(todoCheck, todoInt.tail, acc, false)
        else if (todoCheck.head>todoInt.head && noneIfNextIsGreater) None
        else tailRec(todoCheck.tail, todoInt.tail,
          acc + todoCheck.head*Math.pow(10, todoCheck.tail.size).toInt,
          todoCheck.head == todoInt.head)
      }

      if (digits.size>maxDigits.size) None
      else tailRec(digits, maxDigits, 0, true)
    }

    val minIntAbsoluteString = (-Int.MinValue.toLong).toString
    prepare match {
      case Prepared("", _) => Some(0)
      case Prepared(n, true) if n == minIntAbsoluteString => Some(Int.MinValue)
      case Prepared(n, true) => executeParsing(n).map(-_)
      case Prepared(n, false) => executeParsing(n)
      case _ => None
    }
  }
}
