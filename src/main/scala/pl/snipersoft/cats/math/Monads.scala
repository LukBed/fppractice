package pl.snipersoft.cats.math

import cats.Monad
import cats.implicits._
import cats.instances.option._
import cats.instances.list._
import cats.instances.future._
import cats.syntax.monad._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

//HKT, Monad extends Functor, Monad = Functor(map) + pure + flatMap
object Monads extends App {

  myMonad()
  catsMonad()
  pairsExercise()
  extensionMethods()

  def myMonad(): Unit = {
    trait MyMonad[M[_]] {
      def pure[A](v: A): M[A]
      def flatMap[A, B](mA: M[A])(f: A => M[B]): M[B]
      def map[A, B](mA: M[A])(f: A => B): M[B] = flatMap(mA)(a => pure(f(a)))
    }
  }

  def catsMonad(): Unit =  {
    Monad[Option].pure(4)
    Monad[Option].flatMap(4.some)(x => Some(x + 1))
    Monad[List].pure(3) //List(3)
    Monad[Future].pure(42)
  }

  def pairsExercise(): Unit =  {
    def getPairs[M[_] : Monad, A, B](aa: M[A], bb: M[B]): M[(A, B)] =
      for {
        a <- aa
        b <- bb
      } yield (a, b)

    getPairs(List(1, 2, 3), List('a', 'b', 'c'))
    getPairs(Option(1), Option('a'))
  }

  def extensionMethods(): Unit = {
    import cats.syntax.applicative._
    1.pure[Option].flatMap(x => Some(x+1))
  }

  def functor(): Unit = {
    import cats.Functor
    val functor: Functor[List] = Monad[List]
  }
}
