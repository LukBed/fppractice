package pl.snipersoft.fppractice.graphs

object TownJudge extends App {
  /*
   * There is a town with n people (1 to n).
   * Trust is the list a, b when a trust b.
   * There might be a judge in town.
   * The judge trust nobody.
   * Everybody else trust the judge.
   * There is exactly one person who might be the judge.
   */
  def findJudge(n: Int, trust: List[(Int, Int)]): Option[Int] = {
    val everybody = List.range(1, n+1)

    def isDistrustful(i: Int): Boolean = !trust.exists(_._1 == i)

    def isTrustedByEveryoneElse(maybeJudge: Int): Boolean =
      everybody.forall(person => person == maybeJudge || trust.exists(pair => pair._1 == person && pair._2 == maybeJudge))

    def isPersonPossibleJudge(i: Int): Boolean = isDistrustful(i) && isTrustedByEveryoneElse(i)

    everybody.find(isPersonPossibleJudge)
  }
}
