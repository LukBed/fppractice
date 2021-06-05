package pl.snipersoft.cats.typeclasses

import cats.Applicative
import cats.instances.list._
import cats.instances.option._
import cats.syntax.applicative._
import cats.data.Validated

//functors + the pure method
//monads extend applicatives and applicatives extend functors and semigroupals
//Validated is applicative, but not monad
object Applicatives extends App {

  introduction()

  def introduction() {
    Applicative[List].pure(2) //List(2)
    2.pure[Option] //Some(2)

    type ErrorsOr[T] = Validated[List[String], T]
    val v1: ErrorsOr[Int] = Validated.valid(42)
    val v2: ErrorsOr[Int] = Applicative[ErrorsOr].pure(42)
    val v3: ErrorsOr[Int] = v2.map(_ + 1)
  }

  //really hard!
  def productExercise(): Unit = {
    //W - wrapper

//    def ap[W[_], B, T](wf: W[B => T])(wa: W[B]): W[T] = ???

    def productWithApplicatives[W[_], A, B](wa: W[A], wb: W[B])(implicit applicative: Applicative[W]): W[(A, B)] = {
      val functionWrapper: W[B => (A, B)] = applicative.map(wa)(a => (b: B) => (a, b))
      applicative.ap(functionWrapper)(wb)
    }

    //Applicatives have ap method, so can implements product from Semigroupal
  }
}
