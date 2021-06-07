package pl.snipersoft.cats.typeclasses

import cats.{Eval, Foldable, Monoid}
import cats.instances.list._
import cats.instances.option._

object Folding extends App {

  listFoldExercise()

  def listFoldExercise(): Unit = {
    object ListExercise {
      def map[A, B](list: List[A])(f: A => B): List[B] =
        list.foldRight(List.empty[B])((a, bb) => f(a) :: bb)

      def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] =
        list.foldLeft(List.empty[B])((bb, a) => bb.foldRight(f(a))(_::_))

      def filter[A](list: List[A])(predicate: A => Boolean): List[A] =
        list.foldRight(List.empty[A])((a, aa) => if (predicate(a)) a :: aa else aa)

      def combineAll[A](list: List[A])(implicit monoid: Monoid[A]): A =
        list.foldLeft(monoid.empty)((aa, a) => monoid.combine(aa, a))
    }

    val list = List(1, 2, 3)
    println(ListExercise.map(list)(_ + 1))
    println(ListExercise.flatMap(list)(n => List(n, 10 + n)))
    println(ListExercise.filter(list)(_ != 2))
    println(ListExercise.combineAll(list))
  }

  Foldable[List].foldLeft(List(1,2,3), 0)(_+_) //6
  Foldable[Option].foldLeft(Some(2), 30)(_+_) //32
  Foldable[List].foldRight(List(1,2,3), Eval.now(0)) { (num, eval) => eval.map(_+num) } //Eval(6) - stack safe!
  Foldable[List].combineAll(List(1,2,3)) //6
  Foldable[List].foldMap(List(1,2,3))(_.toString) //"123" - map elements and combine results

  val intsNested = List(Vector(1,2,3), Vector(4,5,6))
  (Foldable[List] compose Foldable[Vector]).combineAll(intsNested) //21

  import cats.syntax.foldable._

  List(1,2,3).combineAll
}
