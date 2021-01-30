package pl.snipersoft.advanced.patternmatching

object ReusePatternMatchingCondition extends App {

  object even {
    def unapply(arg: Int): Boolean = arg % 2 == 0
  }

  object singleDigit {
    def unapply(arg: Int): Boolean = arg < 10 && arg > -10
  }

  def matchNumber(n: Int): String = n match {
      case singleDigit() => "single digit"
      case even() => "an even number"
      case _ => "no property"
    }

  println(matchNumber(12))
  println(matchNumber(9))
  println(matchNumber(21))
}
