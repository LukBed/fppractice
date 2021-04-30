package pl.snipersoft.fppractice.numbers

object LargestNumbers {
  def prepare(numbers: Seq[Int]): Option[String] =
    if (numbers.exists(_ < 0)) None
    else if (numbers.isEmpty) Some("0")
    else Some(prepareForCorrectList(numbers))

  private def prepareForCorrectList(numbers: Seq[Int]): String = {
    implicit val newOrdering: Ordering[Int] = Ordering.fromLessThan { (a, b) =>
      (a.toString + b.toString).compareTo(b.toString + a.toString) >= 0
    }

    numbers.sorted match {
      case 0 :: _ => "0"
      case values => values.mkString("")
    }
  }
}