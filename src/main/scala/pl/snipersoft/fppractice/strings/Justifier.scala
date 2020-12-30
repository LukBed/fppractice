package pl.snipersoft.fppractice.strings

import scala.annotation.tailrec

object Justifier {
  def apply(s: String, width: Int): String = {
    if (s.length <= width || !s.contains(" ")) return s

    val words = s.split(" ").map(_.trim).toList
    val lines = addWordsToLines(words, List(Line(Nil)), width)

    (lines.map(_.value(width)).slice(0, lines.length-1) :+ lines.last.asLastLine()).mkString("\r\n")
  }

  private case class Line(words: List[String]) {
    lazy val length: Int = words match {
      case Nil => 0
      case List(x) => x.length
      case _ => wordLengths + words.length - 1
    }

    lazy val wordLengths = words.map(_.length).sum

    def value(width: Int): String = {
      if (words.size == 1) return words.head

      val spacesNr = spaces(width)
      (0 until words.length - 1).map(i => words(i) + nSpaces(spacesNr.lift(i).getOrElse(0))).mkString("") +
        words.lastOption.getOrElse("")
    }

    def asLastLine(): String = words.mkString(" ")

    def spaces(width: Int): List[Int] = {
      val spaces = width - wordLengths
      val smallSpace = spaces / (words.length - 1)
      val rest = spaces % (words.length - 1)
      (0 until words.length - 1).map(i => if (i < rest) smallSpace + 1 else smallSpace).toList
    }
  }

  @tailrec
  private def addWordsToLines(wordsToBeAdd: List[String], lines: List[Line], width: Int): List[Line] = {
    if (wordsToBeAdd.isEmpty) return lines

    val currentWord = wordsToBeAdd.head
    val currentLine = lines.last

    if (currentLine.length + currentWord.length < width) {
      val newLines = lines.slice(0, lines.length - 1) :+ Line(currentLine.words :+ currentWord)
      addWordsToLines(wordsToBeAdd.tail, newLines, width)
    } else {
      val newLines = lines :+ Line(List(currentWord))
      addWordsToLines(wordsToBeAdd.tail, newLines, width)
    }
  }

  private def nSpaces(n: Int): String =
    n match {
      case 0 => ""
      case _ => (1 to n).map(_ => " ").mkString("")
    }
}