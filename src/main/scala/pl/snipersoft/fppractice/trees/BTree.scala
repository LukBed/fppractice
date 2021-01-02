package pl.snipersoft.fppractice.trees

import scala.annotation.tailrec

//binary tree
sealed abstract class BTree[+T] {
  def value: T
  def left: BTree[T]
  def right: BTree[T]
  def valueOption: Option[T]
  def leftOption: Option[BTree[T]]
  def rightOption: Option[BTree[T]]
  def isEmpty: Boolean

  def isLeaf: Boolean
  def collectLeaves: List[BTree[T]]
  def leafCount: Int = collectLeaves.size
  def size: Int
  def collectNodes(n: Int): List[BTree[T]]
}

case object BEnd extends BTree[Nothing] {
  override def value = throw new NoSuchElementException
  override def left = throw new NoSuchElementException
  override def right = throw new NoSuchElementException
  override def valueOption: Option[Nothing] = None
  override def leftOption: Option[BTree[Nothing]] = None
  override def rightOption: Option[BTree[Nothing]] = None
  override def isEmpty = true
  override def isLeaf = false
  override def collectLeaves: List[BTree[Nothing]] = Nil
  override def size: Int = 0
  override def collectNodes(n: Int): List[Nothing] = Nil
}

case class BNode[+T](override val value: T,
                     override val left: BTree[T] = BEnd,
                     override val right: BTree[T] = BEnd) extends BTree[T] {
  override def valueOption: Option[T] = Some(value)
  override def leftOption: Option[BTree[T]] = Some(left)
  override def rightOption: Option[BTree[T]] = Some(right)
  override def isEmpty = false
  override def isLeaf: Boolean = left.isEmpty && right.isEmpty

  override def collectLeaves: List[BTree[T]] = {
    @tailrec
    def tailRec(remaining: List[BTree[T]], acc: List[BTree[T]]): List[BTree[T]] = {
      remaining match {
        case h :: t if h.isLeaf => tailRec(t, h :: acc)
        case h :: t if h.isEmpty => tailRec(t, acc)
        case h :: t => tailRec(h.left :: h.right :: t, acc)
        case _ => acc
      }
    }

    tailRec(List(this), Nil)
  }

  override def size: Int = {
    @tailrec
    def tailRec(remaining: List[BTree[T]], acc: Int): Int = {
      remaining match {
        case Nil => acc
        case BEnd :: _ => tailRec(remaining.tail, acc)
        case h :: t => tailRec(h.left :: h.right :: t, acc+1)
      }
    }

    tailRec(List(left, right), 0)
  }

  override def collectNodes(n: Int): List[BTree[T]] = {
    @tailrec
    def tailRec(nodes: List[BTree[T]], i: Int): List[BTree[T]] = {
      if (i <= 0) return nodes
      val newNodes = nodes.flatMap(t => List(t.left, t.right)).filter(!_.isEmpty)
      tailRec(newNodes, i-1)
    }

    if (n<0) return Nil
    tailRec(List(this), n)
  }
}