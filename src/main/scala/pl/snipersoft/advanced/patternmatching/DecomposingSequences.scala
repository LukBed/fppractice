package pl.snipersoft.advanced.patternmatching

object DecomposingSequences extends App { //var args pattern
//  val vararg = List(1) match {
//    case List(1, _*) => "starting with one"
//  }

  abstract class MyList[+A] {
    def head: A = ???
    def tail: MyList[A] = ???
  }
  case object Empty extends MyList[Nothing]
  case class Cons[+A](override val head: A, override val tail: MyList[A]) extends MyList[A]

  object MyList {
    def unapplySeq[A](list: MyList[A]): Option[Seq[A]] =
      if (list == Empty) Some(Seq.empty)
      else unapplySeq(list.tail).map(list.head +: _) //+: is :: for sequence
  }

  val myList: MyList[Int] = Cons(1, Cons(2, Cons(3, Empty)))
  val decomposed = myList match {
    case MyList(1, 2, _*) => "starting with 1, 2" //if _* check unapplySeq
    case _ => "starting with something else"
  }

  println(decomposed)
}