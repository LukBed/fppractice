package pl.snipersoft.fppractice

import scala.annotation.tailrec

object RecurringDecimals {
  def apply(numerator: Int, denominator: Int): String = {
    val total = numerator/denominator
    val rest = numerator%denominator

    @tailrec
    def helper(current: Int, operations: List[Operation]): String = {
      val operation = Operation(current, current/denominator, current%denominator)
      val theSameResult = operations.find(o => o.result == operation.result && o.dividend == operation.dividend)

      theSameResult match {
        case Some(op) => splitOperationsResults(operations.indexOf(op), operations)
        case _ => helper(operation.rest*10, operations :+ operation)
      }
    }

    def splitOperationsResults(splitIndex: Int, operations: List[Operation]): String = {
      val before = operations.slice(0, splitIndex).map(_.result).mkString("")
      val after = operations.slice(splitIndex, operations.size).map(_.result).mkString("")

      after match {
        case "0" => before
        case _ => s"$before($after)"
      }
    }

    rest match {
      case 0 => total.toString
      case _ => s"$total.${helper(rest*10, Nil)}"
    }
  }

  private case class Operation(dividend: Int, result: Int, rest: Int)
}
