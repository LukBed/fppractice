package pl.snipersoft.fppractice.graphs

import scala.annotation.tailrec

object Graphs extends App {
  type Graph[T] = Map[T, Set[T]]

  /**
   * Number of nodes this node is associated (adjacent) to.
   */
  def outDegree[T](graph: Graph[T], node: T): Int = graph.get(node).map(_.size).getOrElse(0)

  /**
   * Number of nodes connected to node.
   */
  def inDegree[T](graph: Graph[T], node: T): Int = graph.values.count(_.contains(node))

  /**
   * Check is path between two nodes in graph.
   */
  def isPath[T](graph: Graph[T], from: T, to: T): Boolean = {

    @tailrec
    def tailRec(toCheck: Set[T], checked: Set[T]): Boolean =
      if (toCheck.isEmpty) false
      else if (toCheck.head == to) true
      else {
        val current = toCheck.head
        if (checked.contains(current)) tailRec(toCheck.tail, checked)
        else tailRec(toCheck.tail ++ graph.getOrElse(current, Set()), checked + current)
      }

    val toCheck = graph.getOrElse(from, Set())
    tailRec(toCheck, Set())
  }
}
