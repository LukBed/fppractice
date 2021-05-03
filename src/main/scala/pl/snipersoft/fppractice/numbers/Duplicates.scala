package pl.snipersoft.fppractice.numbers

object Duplicates {
  /**
   * Input is list of integers. All numbers in the list appear exactly twice, except one number.
   * Returning this one number.
   */
  def findNotDuplicatedNumber(list: List[Int]): Int = {
    /*
    ^ - XOR
    If a is Int, then a^a=0
    0^a=a
     */

    list.foldLeft(0)(_ ^ _)
  }

}
