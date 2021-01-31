package pl.snipersoft.advanced.functionalcollection

import scala.annotation.tailrec

sealed trait MySet[A] extends (A => Boolean) {
  def contains(elem: A): Boolean
  def +(elem: A): MySet[A]
  def ++(anotherSet: MySet[A]): MySet[A]
  def -(elem: A): MySet[A]

  def map[B](f: A => B): MySet[B]
  def flatMap[B](f: A => MySet[B]): MySet[B]
  def filter(predicate: A => Boolean): MySet[A]
  def foreach(f: A => Unit): Unit

  def intersect(anotherSet: MySet[A]): MySet[A]
  def difference(anotherSet: MySet[A]): MySet[A]
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

private case class EmptySet[A]() extends MySet[A] {
  override def apply(elem: A): Boolean = contains(elem)

  override def contains(elem: A) = false
  override def +(elem: A): MySet[A] = NonEmptySet(elem, this)
  override def ++(anotherSet: MySet[A]): MySet[A] = anotherSet
  override def -(elem: A): MySet[A] = this

  override def map[B](f: A => B): EmptySet[B] = EmptySet()
  override def flatMap[B](f: A => MySet[B]): MySet[B] = EmptySet()
  override def filter(predicate: A => Boolean): MySet[A] = EmptySet()
  override def foreach(f: A => Unit): Unit = ()

  override def intersect(anotherSet: MySet[A]): MySet[A] = this
  override def difference(anotherSet: MySet[A]): MySet[A] = anotherSet
}

private case class NonEmptySet[A](head: A, tail: MySet[A]) extends MySet[A] {
  override def apply(elem: A): Boolean = contains(elem)

  override def contains(elem: A): Boolean = elem == head || tail.contains(elem)

  override def +(elem: A): MySet[A] =
    if (this contains elem) this
    else NonEmptySet(elem, this)

  override def ++(anotherSet: MySet[A]): MySet[A] = tail ++ anotherSet + head

  override def -(elem: A): MySet[A] =
    if (head == elem) tail - elem
    else (tail - elem) + head

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

  override def intersect(anotherSet: MySet[A]): MySet[A] = {
    @tailrec
    def tailRec(toCheck: MySet[A], acc: MySet[A]): MySet[A] = {
      toCheck match {
        case NonEmptySet(h, t) if anotherSet(h) => tailRec(t, acc + h)
        case NonEmptySet(_, t) => tailRec(t, acc)
        case EmptySet() => acc
      }
    }

    tailRec(this, EmptySet())
  }

  override def difference(anotherSet: MySet[A]): MySet[A] = {
    @tailrec
    def tailRec(toCheck: MySet[A], acc: MySet[A]): MySet[A] = {
      toCheck match {
        case NonEmptySet(h, t) if this(h) && anotherSet(h) => tailRec(t, acc)
        case NonEmptySet(h, t) => tailRec(t, acc + h)
        case EmptySet() => acc
      }
    }

    tailRec(this ++ anotherSet, EmptySet())
  }
}