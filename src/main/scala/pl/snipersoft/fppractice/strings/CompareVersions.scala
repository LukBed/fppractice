package pl.snipersoft.fppractice.strings

import scala.annotation.tailrec

object CompareVersions {
  /**
   * Returning:
   * -1 if version A is older than version B
   * 1 if version A is newest than version B
   * 0 if version A is equal to version B
   */
  def compare(versionA: String, versionB: String): Int = {
    def readSegments(version: String): List[Int] = List.from(version.split('.')).map(_.toInt)

    @tailrec
    def tailRec(segmentsA: List[Int], segmentsB: List[Int]): Int = {
      def readValue(segments: List[Int]): Int = segments.headOption.getOrElse(0)
      def readSafeTail(segments: List[Int]) = if (segments.isEmpty) Nil else segments.tail

      val valueA = readValue(segmentsA)
      val valueB = readValue(segmentsB)
      val predicted = if (valueA>valueB) 1 else if (valueA<valueB) -1 else 0

      if (segmentsA.isEmpty && segmentsB.isEmpty) 0
      else if (predicted != 0) predicted
      else tailRec(readSafeTail(segmentsA), readSafeTail(segmentsB))
    }

    val segmentsA = readSegments(versionA)
    val segmentsB = readSegments(versionB)
    tailRec(segmentsA, segmentsB)
  }
}
