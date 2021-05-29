package pl.snipersoft.cats.math

import cats.Functor
import cats.implicits._
import cats.instances.list._
import cats.instances.option._
import cats.instances.try_._
import cats.syntax.functor._

import scala.util.Try

//HKT, generalize map function
object Functors extends App {

  simplifiedDefinition()
  generalizingApi()
  binaryTreeExercise()

  def simplifiedDefinition() {
    trait MyFunctor[F[_]] {
      def map[A, B](initialValue: F[A])(f: A => B): F[B] //like in Cats
    }
  }

  def do10x[F[_]](value: F[Int])(implicit functor: Functor[F]): F[Int] = functor.map(value)(_*10)

  def generalizingApi() {
    Functor[List].map(List(1, 2, 3))(_ + 1) //List(2,3,4)
    Functor[Option].map(5.some)(_ * 2) //Some(10)
    Functor[Try].map(Try(6))(_ - 1) //Success(5)

    //we use Functors to generalizing an API
    def do10xList(list: List[Int]): List[Int] = list.map(_*10)
    def do10xOption(option: Option[Int]): Option[Int] = option.map(_*10)
    def do10xTry(attempt: Try[Int]): Try[Int] = attempt.map(_*10)

    println(do10x(List(1,2,3)))
    println(do10x(4.some))
    println(do10x(Try(5)))
  }

  def binaryTreeExercise(): Unit = {
    trait Tree[+T]
    //best practise - smart constructors
    object Tree {
      def leaf[T](value: T): Tree[T] = Leaf(value)
      def branch[T](value: T, left: Tree[T], right: Tree[T]): Tree[T] = Branch(value, left, right)
    }
    import Tree._

    case class Leaf[+T](value: T) extends Tree[T]
    case class Branch[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T]

    //stack recursive - we will back to this!
    implicit object TreeFunctor extends Functor[Tree] {
      override def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
        case Leaf(value) => Leaf(f(value))
        case Branch(value, left, right) => Branch(f(value), map(left)(f), map(right)(f))
      }
    }

    println(do10x[Tree](Branch(1, Leaf(2), Leaf(3)))) //case class constructor
    println(do10x(branch(1, leaf(2), leaf(3)))) //smart constructor

    val tree = branch(1, leaf(2), leaf(3))
    import cats.syntax.functor._
    println(tree.map(_+1))
  }

  def shorterDo10xExercise(): Unit = {
    import cats.syntax.functor._
    def do10x[F[_] : Functor](value: F[Int]): F[Int] = value.map(_*10)
  }
}
