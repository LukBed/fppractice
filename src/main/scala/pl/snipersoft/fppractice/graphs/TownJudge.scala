package pl.snipersoft.fppractice.graphs

object TownJudge {

  type Trust = (Int, Int)

  /*
   * There is a town with n people (1 to n).
   * Trust is the list a, b when a trust b.
   * There might be a judge in town.
   * The judge trust nobody.
   * Everybody else trust the judge.
   * There is exactly one person who might be the judge.
   */
  def findJudge(n: Int, trust: List[Trust]): Option[Int] = {

    val outDegrees: Map[Int, Int] = trust.foldLeft(Map[Int, Int]()) {
      case (map, (trusting, _)) => map + (trusting -> (1 + map.getOrElse(trusting, 0)))
    }

    val inDegrees: Map[Int, Int] = trust.foldLeft(Map[Int, Int]()) {
      case (map, (_, trusted)) => map + (trusted -> (1 + map.getOrElse(trusted, 0)))
    }

    def isDistrustful(maybeJudge: Int): Boolean = outDegrees.getOrElse(maybeJudge, 0) == 0

    def isTrustedByEveryoneElse(maybeJudge: Int): Boolean = inDegrees.get(maybeJudge).contains(n - 1)

    (1 to n).find(maybeJudge => isDistrustful(maybeJudge) && isTrustedByEveryoneElse(maybeJudge))
  }
}
