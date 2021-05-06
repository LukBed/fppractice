package pl.snipersoft.fppractice.graphs

import pl.snipersoft.fppractice.graphs.Graphs.Graph

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
    def trustedBy(i: Int): Set[Int] = trust.filter(pair => pair._1 == i && pair._2 != i).map(_._2).toSet

    val graph: Graph[Int] = Range(1, n+1).map(i => i -> trustedBy(i)).toMap

    def isDistrustful(maybeJudge: Int): Boolean = Graphs.outDegree(graph, maybeJudge) == 0
    def isTrustedByEveryoneElse(maybeJudge: Int): Boolean = Graphs.inDegree(graph, maybeJudge) == n-1

    graph.keys.find(maybeJudge => isDistrustful(maybeJudge) && isTrustedByEveryoneElse(maybeJudge))
  }
}
