package pl.snipersoft.fppractice.lists

import scala.annotation.tailrec

sealed abstract class MyList[+T] {
  def head: T
  def tail: MyList[T]
  def isEmpty: Boolean = headOption.isEmpty
  def headOption: Option[T]
  def ::[S >: T](elem: S): MyList[S] = new ::(elem, this)
  def apply(index: Int): T
  def length: Int
}

case object MyNil extends MyList[Nothing] {
  override def head: Nothing = throw new NoSuchElementException
  override def tail = throw new NoSuchElementException
  override def headOption: Option[Nothing] = None
  override def toString: String = "[]"
  override def apply(index: Int) = throw new NoSuchElementException
  override def length: Int = 0
}

case class ::[+T](override val head: T, override val tail: MyList[T]) extends MyList[T] {
  override def headOption: Option[T] = Some(head)

  override def toString: String = {
    @tailrec
    def tailRec(remaining: MyList[T], result: String): String = {
      if (remaining.isEmpty) result
      else tailRec(remaining.tail, s"$result${remaining.head};")
    }

    val generated = tailRec(this, "")
    s"[${generated.substring(0, generated.length-1)}]"
  }

  override def apply(index: Int): T = {
    @tailrec
    def tailRec(list: MyList[T], n: Int): T = {
      if (n == 0) return list.head
      if (list.isEmpty) throw new NoSuchElementException
      tailRec(list.tail, n-1)
    }

    tailRec(this, index)
  }

  override def length: Int = {
    @tailrec
    def tailRec(list: MyList[T], l: Int): Int = {
      if (list.isEmpty) return l
      tailRec(list.tail, l+1)
    }

    tailRec(this, 0)
  }
}