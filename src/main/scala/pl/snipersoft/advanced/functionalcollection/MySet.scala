package pl.snipersoft.advanced.functionalcollection

import scala.annotation.tailrec

sealed trait MySet[A] extends (A => Boolean) {
  def contains(elem: A): Boolean
  def +(elem: A): MySet[A]
  def ++(anotherSet: MySet[A]): MySet[A]

  def map[B](f: A => B): MySet[B]
  def flatMap[B](f: A => MySet[B]): MySet[B]
  def filter(predicate: A => Boolean): MySet[A]
  def foreach(f: A => Unit): Unit
}

case class EmptySet[A]() extends MySet[A] {
  override def apply(elem: A): Boolean = contains(elem)

  override def contains(elem: A) = false
  override def +(elem: A): MySet[A] = NonEmptySet(elem, EmptySet())
  override def ++(anotherSet: MySet[A]): MySet[A] = anotherSet

  override def map[B](f: A => B): EmptySet[B] = EmptySet()
  override def flatMap[B](f: A => MySet[B]): MySet[B] = EmptySet()
  override def filter(predicate: A => Boolean): MySet[A] = EmptySet()
  override def foreach(f: A => Unit): Unit = {}
}

case class NonEmptySet[A](head: A, tail: MySet[A]) extends MySet[A] {
  override def apply(elem: A): Boolean = contains(elem)

  override def contains(elem: A): Boolean = {
    @tailrec
    def tailRec(remaining: MySet[A]): Boolean = {
      remaining match {
        case NonEmptySet(h, _) if h == elem => true
        case NonEmptySet(_, t)  => tailRec(t)
        case EmptySet() => false
      }
    }

    tailRec(this)
  }

  override def +(elem: A): MySet[A] =
    if (this(elem)) this
    else NonEmptySet(elem, this)

  override def ++(anotherSet: MySet[A]): MySet[A] = {
    @tailrec
    def tailRec(remaining: MySet[A], acc: MySet[A]): MySet[A] = {
      remaining match {
        case EmptySet() => acc
        case NonEmptySet(h, t) => tailRec(t, acc + h)
      }
    }

    tailRec(anotherSet, this)
  }

  override def map[B](f: A => B): MySet[B] = {
    @tailrec
    def tailRec(remaining: MySet[A], acc: MySet[B]): MySet[B] = {
      remaining match {
        case EmptySet() => acc
        case NonEmptySet(h, t) => tailRec(t, acc + f(h))
      }
    }

    tailRec(this, EmptySet())
  }

  override def flatMap[B](f: A => MySet[B]): MySet[B] = {
    @tailrec
    def tailRec(remaining: MySet[A], acc: MySet[B]): MySet[B] = {
      remaining match {
        case EmptySet() => acc
        case NonEmptySet(h, t) => tailRec(t, acc ++ f(h))
      }
    }

    tailRec(this, EmptySet())
  }

  override def filter(predicate: A => Boolean): MySet[A] = {
    @tailrec
    def tailRec(remaining: MySet[A], acc: MySet[A]): MySet[A] = {
      remaining match {
        case EmptySet() => acc
        case NonEmptySet(h, t) if predicate(h) => tailRec(t, acc + h)
        case NonEmptySet(_, t) => tailRec(t, acc)
      }
    }

    tailRec(this, EmptySet())
  }

  override def foreach(f: A => Unit): Unit = {
    @tailrec
    def tailRec(remaining: MySet[A]): Unit = {
      remaining match {
        case NonEmptySet(h, t) =>
          f(h)
          tailRec(t)
        case _ =>
      }
    }

    tailRec(this)
  }
}