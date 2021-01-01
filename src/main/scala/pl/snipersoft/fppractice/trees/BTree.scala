package pl.snipersoft.fppractice.trees

//binary tree
sealed abstract class BTree[+T] {
  def value: T
  def left: BTree[T]
  def right: BTree[T]
  def valueOption: Option[T]
  def leftOption: Option[BTree[T]]
  def rightOption: Option[BTree[T]]
  def isEmpty: Boolean

  //leaf is tree without node
  def isLeaf: Boolean
  def collectLeaves: List[BTree[T]]
  def leafCount: Int
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
  override def collectLeaves: List[BTree[Nothing]] = ???
  override def leafCount: Int = ???
}

case class BNode[+T](override val value: T,
                     override val left: BTree[T] = BEnd,
                     override val right: BTree[T] = BEnd) extends BTree[T] {
  override def valueOption: Option[T] = Some(value)
  override def leftOption: Option[BTree[T]] = Some(left)
  override def rightOption: Option[BTree[T]] = Some(right)
  override def isEmpty = false
  override def isLeaf: Boolean = left.isEmpty && right.isEmpty
  override def collectLeaves: List[BTree[T]] = ???
  override def leafCount: Int = ???
}