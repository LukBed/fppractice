package pl.snipersoft.cats.typeclasses

import cats.{Functor, Semigroupal}

object WeakerApplicatives extends App {
  trait MyApply[W[_]] extends Functor[W] with Semigroupal[W] {
    override def product[A, B](fa: W[A], fb: W[B]): W[(A, B)] = {
      val functionWrapper: W[B => (A, B)] = map(fa)(a => (b: B) => (a, b))
      ap(functionWrapper)(fb)
    }

    //fundamental method - very abstract
    def ap[A, B](wf: W[A => B])(wa: W[A]): W[B] = ???

    //exercise
    def mapN[A, B, C](tuple: (W[A], W[B]))(f: (A, B) => C): W[C] = {
      val wrappedTuple: W[(A, B)] = product(tuple._1, tuple._2)
      map(wrappedTuple)(pair => f(pair._1, pair._2))
    }

  }

  trait MyApplicative[W[_]] extends MyApply[W] {
    //fundamental method
    def pure[A](a: A): W[A]
  }

  import cats.Apply
  import cats.instances.option._
  Apply[Option].ap(Some((x: Int) => x+1))(Some(2)) //Some(3)

  import cats.syntax.apply._
  val tupleOfOptions = (Option(1), Option(2), Option(3))
  val optionOfTuple = tupleOfOptions.tupled //Some(1, 2, 3)
  val sumOptions = tupleOfOptions.mapN(_ + _ + _) //Some(6)
}
