package pl.snipersoft.cats.typeclasses

import cats.{Applicative, Apply}

object WeakerMonads extends App {

  trait MyFlatMap[M[_]] extends Apply[M] {
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

    def ap[A, B](wf: M[A => B])(wa: M[A]): M[B] =
      flatMap(wa)(a => map(wf)(fab => fab(a)))
  }

  trait MyMonad[M[_]] extends Applicative[M] with MyFlatMap[M] {
    override def map[A, B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(a => pure(f(a)))
  }

  import cats.FlatMap
  import cats.syntax.flatMap._ //flatMap extension method
  import cats.syntax.functor._ //map extension method

  //we can use for comprehension with just FlatMap,we don't need Monad
  def getPairs[M[_] : FlatMap, A, B](v1: M[A], v2: M[B]): M[(A, B)] = for {
    a <- v1
    b <- v2
  } yield (a, b)
}
