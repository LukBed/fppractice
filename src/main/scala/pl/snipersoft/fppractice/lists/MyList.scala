package pl.snipersoft.fppractice.lists

import scala.annotation.tailrec
import scala.util.Random

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

  def duplicateEach(n: Int): MyList[T]

  def rotate(n: Int): MyList[T]

  def sample(n: Int): MyList[T]

  def insertionSorted[S >: T](ordering: Ordering[S]): MyList[S]

  def mergeSorted[S >: T](ordering: Ordering[S]): MyList[S]

  def quickSorted[S >: T](ordering: Ordering[S]): MyList[S]
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

  override def duplicateEach(n: Int): MyList[Nothing] = this

  override def rotate(n: Int): MyList[Nothing] = this

  override def sample(n: Int): MyList[Nothing] = this

  override def insertionSorted[S >: Nothing](ordering: Ordering[S]): MyList[Nothing] = this

  override def mergeSorted[S >: Nothing](ordering: Ordering[S]): MyList[Nothing] = this

  override def quickSorted[S >: Nothing](ordering: Ordering[S]): MyList[Nothing] = this
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
      tailRec(todo.tail, todo.head :: acc, n - 1)
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
      if (todo.isEmpty) return acc
      val headList = f(todo.head)
      val accWithHeadList = addElements(headList, acc)
      tailRec(todo.tail, accWithHeadList)
    }

    @tailrec
    def addElements[U](todo: MyList[U], acc: MyList[U]): MyList[U] = {
      if (todo.isEmpty) return acc
      addElements(todo.tail, todo.head :: acc)
    }

    tailRec(this, MyNil).reverse
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
      if (todo.isEmpty) return checked ++ ((element, 1) :: MyNil)
      if (todo.head._1 == element) {
        val incremented = todo.head.copy(_2 = todo.head._2 + 1)
        return (incremented :: MyNil) ++ todo.tail ++ checked.reverse
      }

      updateCountTailRec(element, todo.tail, todo.head :: checked)
    }

    tailRec(this, MyNil)
  }

  override def duplicateEach(n: Int): MyList[T] = {
    def duplicateElement(elem: T): MyList[T] = duplicateElementTailRec(elem, n, MyNil)

    @tailrec
    def duplicateElementTailRec(elem: T, i: Int, acc: MyList[T]): MyList[T] = {
      if (i == 0) return acc
      duplicateElementTailRec(elem, i - 1, elem :: acc)
    }

    flatMap(duplicateElement)
  }

  override def rotate(n: Int): MyList[T] = {
    @tailrec
    def tailRec(i: Int, todo: MyList[T], acc: MyList[T]): MyList[T] = {
      if (i == 0) return todo ++ acc.reverse
      if (todo.isEmpty) throw new IllegalArgumentException
      tailRec(i - 1, todo.tail, todo.head :: acc)
    }

    if (n < 0) throw new IllegalArgumentException
    tailRec(n, this, MyNil)
  }

  override def sample(n: Int): MyList[T] = {
    val random = new Random(System.currentTimeMillis())
    val l = length
    MyList.from(0 until n).map(_ => apply(random.nextInt(l)))
  }

  override def insertionSorted[S >: T](ordering: Ordering[S]): MyList[S] = {
    @tailrec
    def tailRec(todo: MyList[S], acc: MyList[S]): MyList[S] = {
      if (todo.isEmpty) return acc
      tailRec(todo.tail, addElement(todo.head, acc))
    }

    @tailrec
    def addElement(elem: S, todo: MyList[S], acc: MyList[S] = MyNil): MyList[S] = {
      if (todo.isEmpty) return (elem :: acc).reverse
      if (ordering.gteq(todo.head, elem)) return (elem :: acc).reverse ++ todo
      addElement(elem, todo.tail, todo.head :: acc)
    }

    tailRec(this, MyNil)
  }

  override def mergeSorted[S >: T](ordering: Ordering[S]): MyList[S] = {
    @tailrec
    def tailRec(smallLists: MyList[MyList[S]], bigLists: MyList[MyList[S]]): MyList[S] = {
      (smallLists, bigLists) match {
        case (MyNil, MyNil) => MyNil
        case (MyNil, _ :: MyNil) => bigLists.head
        case (MyNil, _) => tailRec(bigLists, MyNil)
        case (_ :: MyNil, _) => tailRec(smallLists.head :: bigLists, MyNil)
        case _ => val newList = join(smallLists.head, smallLists.tail.head)
          tailRec(smallLists.tail.tail, newList :: bigLists)
      }
    }

    def join(l1: MyList[S], l2: MyList[S]): MyList[S] = {
      @tailrec
      def addElement(elem: S, todo: MyList[S], acc: MyList[S] = MyNil): MyList[S] = {
        if (todo.isEmpty) return (elem :: acc).reverse
        if (ordering.gteq(todo.head, elem)) return (elem :: acc).reverse ++ todo
        addElement(elem, todo.tail, todo.head :: acc)
      }

      @tailrec
      def addAllElements(todo: MyList[S], acc: MyList[S]): MyList[S] = {
        if (todo.isEmpty) return acc
        addAllElements(todo.tail, addElement(todo.head, acc))
      }

      addAllElements(l2, l1)
    }

    if (tail.isEmpty) return this
    tailRec(this.map(_ :: MyNil), MyNil)
  }

  override def quickSorted[S >: T](ordering: Ordering[S]): MyList[S] = {
    @tailrec
    def readLeftAndRight(elem: S, toCheck: MyList[S],
                         left: MyList[S] = MyNil, right: MyList[S] = MyNil): (MyList[S], MyList[S]) = {
      if (toCheck.isEmpty) return (left, right)
      if (ordering.gteq(toCheck.head, elem)) readLeftAndRight(elem, toCheck.tail, left, toCheck.head :: right)
      else readLeftAndRight(elem, toCheck.tail, toCheck.head :: left, right)
    }

    val leftAndRight = readLeftAndRight(head, tail)
    val left = leftAndRight._1
    val right = leftAndRight._2
    left.quickSorted(ordering) ++ (head :: right.quickSorted(ordering))
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