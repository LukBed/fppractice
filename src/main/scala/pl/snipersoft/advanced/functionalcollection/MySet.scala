package pl.snipersoft.advanced.functionalcollection

import scala.annotation.tailrec

sealed trait MySet[A] extends (A => Boolean) {
  def apply(elem: A): Boolean = contains(elem)

  def unary_! : MySet[A]

  def contains(elem: A): Boolean
  def +(elem: A): MySet[A]
  def ++(anotherSet: MySet[A]): MySet[A] //union

  def -(elem: A): MySet[A]
  def --(anotherSet: MySet[A]): MySet[A] //difference
  def &(anotherSet: MySet[A]): MySet[A] //intersection

  def map[B](f: A => B): MySet[B]
  def flatMap[B](f: A => MySet[B]): MySet[B]
  def filter(predicate: A => Boolean): MySet[A]
  def foreach(f: A => Unit): Unit
}

object MySet {
  def apply[A](values: A*): MySet[A] = {
    @tailrec
    def buildSet(valSeq: Seq[A], acc: MySet[A]): MySet[A] =
      if (valSeq.isEmpty) acc
      else buildSet(valSeq.tail, acc + valSeq.head)

    buildSet(values.toSeq, EmptySet())
  }
}

case class EmptySet[A]() extends MySet[A] {
  override def unary_! : MySet[A] = PropertyBasedSet(_ => true)

  override def contains(elem: A) = false
  override def +(elem: A): MySet[A] = NonEmptySet(elem, this)
  override def ++(anotherSet: MySet[A]): MySet[A] = anotherSet

  override def -(elem: A): MySet[A] = this
  override def --(anotherSet: MySet[A]): MySet[A] = this
  override def &(anotherSet: MySet[A]): MySet[A] = this

  override def map[B](f: A => B): EmptySet[B] = EmptySet()
  override def flatMap[B](f: A => MySet[B]): MySet[B] = EmptySet()
  override def filter(predicate: A => Boolean): MySet[A] = EmptySet()
  override def foreach(f: A => Unit): Unit = ()
}

case class NonEmptySet[A](head: A, tail: MySet[A]) extends MySet[A] {
  override def unary_! : MySet[A] = PropertyBasedSet(x => !this(x))

  override def contains(elem: A): Boolean = elem == head || tail.contains(elem)

  override def +(elem: A): MySet[A] =
    if (this contains elem) this
    else NonEmptySet(elem, this)

  override def ++(anotherSet: MySet[A]): MySet[A] = tail ++ anotherSet + head

  override def -(elem: A): MySet[A] =
    if (head == elem) tail - elem
    else tail - elem + head

  override def &(anotherSet: MySet[A]): MySet[A] = filter(anotherSet)

  override def --(anotherSet: MySet[A]): MySet[A] = filter(x => !anotherSet(x))

  override def map[B](f: A => B): MySet[B] = (tail map f) + f(head)

  override def flatMap[B](f: A => MySet[B]): MySet[B] = (tail flatMap f) ++ f(head)

  override def filter(predicate: A => Boolean): MySet[A] = {
    val filteredTail = tail filter predicate
    if (predicate(head)) filteredTail + head
    else filteredTail
  }

  override def foreach(f: A => Unit): Unit = {
    f(head)
    tail foreach f
  }
}

case class PropertyBasedSet[A](property: A => Boolean) extends MySet[A] {
  override def unary_! : MySet[A] = PropertyBasedSet(x => !this(x))

  override def contains(elem: A): Boolean = property(elem)
  override def +(elem: A): MySet[A] = if (this(elem)) this else PropertyBasedSet(x => this(x) || elem == x)
  override def ++(anotherSet: MySet[A]): MySet[A] = PropertyBasedSet(x => this(x) || anotherSet(x))

  override def -(elem: A): MySet[A] = filter(x => x != elem)
  override def --(anotherSet: MySet[A]): MySet[A] = filter(!anotherSet)
  override def &(anotherSet: MySet[A]): MySet[A] = filter(anotherSet)

  override def map[B](f: A => B): MySet[B] = politelyFail
  override def flatMap[B](f: A => MySet[B]): MySet[B] = politelyFail
  override def filter(predicate: A => Boolean): MySet[A] = PropertyBasedSet(x => this(x) || predicate(x))

  override def foreach(f: A => Unit): Unit = politelyFail

  private def politelyFail = throw new IllegalArgumentException("Really deep rabit hole")
}
