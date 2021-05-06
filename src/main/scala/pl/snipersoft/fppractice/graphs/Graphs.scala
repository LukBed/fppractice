package pl.snipersoft.fppractice.graphs

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
}
