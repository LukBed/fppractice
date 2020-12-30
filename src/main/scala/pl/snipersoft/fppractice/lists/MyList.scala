package pl.snipersoft.fppractice.lists

import scala.annotation.tailrec

sealed abstract class MyList[+T] {
  def head: T

  def tail: MyList[T]

  def lastOption: Option[T]

  def isEmpty: Boolean = headOption.isEmpty

  def headOption: Option[T]

  def ::[S >: T](elem: S): MyList[S] = new ::(elem, this)

  def apply(index: Int): T

  def length: Int

  def reverse: MyList[T]

  def addAtTheEnd[S >: T](elem: S): MyList[S]

  def ++[S >: T](other: MyList[S]): MyList[S]

  def removeAt(index: Int): MyList[T]

  def map[S](f: T => S): MyList[S]

  def flatMap[S](f: T => MyList[S]): MyList[S]

  def filter(f: T => Boolean): MyList[T]

  def rle(): MyList[(T, Int)]
}

case object MyNil extends MyList[Nothing] {
  override def head: Nothing = throw new NoSuchElementException

  override def tail = throw new NoSuchElementException

  override def lastOption: Option[Nothing] = None

  override def headOption: Option[Nothing] = None

  override def toString: String = "[]"

  override def apply(index: Int) = throw new NoSuchElementException

  override def length: Int = 0

  override def reverse: MyList[Nothing] = this

  override def addAtTheEnd[S >: Nothing](elem: S): MyList[S] = elem :: this

  override def ++[S >: Nothing](other: MyList[S]): MyList[S] = other

  override def removeAt(index: Int): MyList[Nothing] = this

  override def map[S](f: Nothing => S): MyList[Nothing] = this

  override def flatMap[S](f: Nothing => MyList[S]): MyList[Nothing] = this

  override def filter(f: Nothing => Boolean): MyList[Nothing] = this

  override def rle(): MyList[Nothing] = this
}

case class ::[+T](override val head: T, override val tail: MyList[T]) extends MyList[T] {
  override def headOption: Option[T] = Some(head)

  override def lastOption: Option[T] = if (tail.isEmpty) headOption else tail.lastOption

  override def toString: String = {
    @tailrec
    def tailRec(remaining: MyList[T], result: String): String = {
      if (remaining.isEmpty) result
      else tailRec(remaining.tail, s"$result${remaining.head};")
    }

    val generated = tailRec(this, "")
    s"[${generated.substring(0, generated.length - 1)}]"
  }

  override def apply(index: Int): T = {
    @tailrec
    def tailRec(list: MyList[T], n: Int): T = {
      if (n == 0) return list.head
      if (list.isEmpty) throw new NoSuchElementException
      tailRec(list.tail, n - 1)
    }

    tailRec(this, index)
  }

  override def length: Int = {
    @tailrec
    def tailRec(list: MyList[T], l: Int): Int = {
      if (list.isEmpty) return l
      tailRec(list.tail, l + 1)
    }

    tailRec(this, 0)
  }

  override def reverse: MyList[T] = {
    @tailrec
    def tailRec(todo: MyList[T], acc: MyList[T]): MyList[T] = {
      if (todo.isEmpty) return acc
      tailRec(todo.tail, todo.head :: acc)
    }

    tailRec(this, MyNil)
  }

  override def addAtTheEnd[S >: T](elem: S): MyList[S] = {
    if (tail.isEmpty) return head :: elem :: MyNil
    if (tail.tail.isEmpty) return head :: tail.head :: elem :: MyNil
    head :: tail.addAtTheEnd(elem)
  }

  override def ++[S >: T](other: MyList[S]): MyList[S] = {
    @tailrec
    def tailRec(toAdd: MyList[S], acc: MyList[S]): MyList[S] = {
      if (toAdd.isEmpty) return acc
      tailRec(toAdd.tail, toAdd.head :: acc)
    }

    tailRec(this.reverse, other)
  }

  override def removeAt(index: Int): MyList[T] = {
    @tailrec
    def tailRec(todo: MyList[T], acc: MyList[T], n: Int): MyList[T] = {
      if (todo.isEmpty) return this
      if (n == 0) return acc.reverse ++ todo.tail
      tailRec(todo.tail, todo.head :: acc, n-1)
    }

    if (index < 0) return this
    tailRec(this, MyNil, index)
  }

  override def map[S](f: T => S): MyList[S] = {
    @tailrec
    def tailRec(todo: MyList[T], acc: MyList[S]): MyList[S] = {
      if (todo.isEmpty) return acc
      tailRec(todo.tail, f(todo.head) :: acc)
    }

    tailRec(this, MyNil).reverse
  }

  override def flatMap[S](f: T => MyList[S]): MyList[S] = {
    @tailrec
    def tailRec(todo: MyList[T], acc: MyList[S]): MyList[S] = {
      if (todo.isEmpty) return acc.reverse
      tailRec(todo.tail, f(todo.head).reverse ++ acc)
    }

    tailRec(this, MyNil)
  }

  override def filter(f: T => Boolean): MyList[T] = {
    @tailrec
    def tailRec(todo: MyList[T], acc: MyList[T]): MyList[T] = {
      if (todo.isEmpty) return acc
      if (f(todo.head)) tailRec(todo.tail, todo.head :: acc)
      else tailRec(todo.tail, acc)
    }

    tailRec(this, MyNil).reverse
  }

  override def rle(): MyList[(T, Int)] = {
    @tailrec
    def tailRec(todo: MyList[T], acc: MyList[(T, Int)]): MyList[(T, Int)] = {
      if (todo.isEmpty) return acc
      tailRec(todo.tail, updateCount(todo.head, acc))
    }

    def updateCount(element: T, acc: MyList[(T, Int)]): MyList[(T, Int)] = {
      updateCountTailRec(element, acc, MyNil)
    }

    @tailrec
    def updateCountTailRec(element: T, todo: MyList[(T, Int)], checked: MyList[(T, Int)]): MyList[(T, Int)] = {
      if (todo.isEmpty) return (element, 1) :: checked
      if (todo.head._1 == element) {
        val incremented = todo.head.copy(_2 = todo.head._2 + 1)
        return checked.reverse ++ (incremented :: MyNil) ++ todo.tail
      }

      updateCountTailRec(element, todo.tail, todo.head :: checked)
    }

    tailRec(this, MyNil)
  }
}

object MyList {
  def from[T](iterable: Iterable[T]): MyList[T] = {
    @tailrec
    def tailRec(i: Iterable[T], acc: MyList[T]): MyList[T] = {
      if (iterable.isEmpty) return acc
      if (i.size == 1) return i.head :: acc
      tailRec(i.tail, i.head :: acc)
    }

    tailRec(iterable, MyNil).reverse
  }
}