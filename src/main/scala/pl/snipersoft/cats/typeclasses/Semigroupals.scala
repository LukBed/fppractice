package pl.snipersoft.cats.typeclasses

import cats.Semigroupal
import cats.instances.option._
import cats.instances.future._
import cats.instances.list._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

//Monads extend Semigroupals, but Semogroupal doesn't have to be monad and fulfill monad laws ex. Validated
//products two wrapped values into wrapped tuple
object Semigroupals extends App {

  introduction()
  productExercise()
  validated()
  zipSemigroupal()

  def introduction(): Unit = {
    trait MySemigroupal[F[_]] {
      def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
    }

    Semigroupal[Option].product(Some(42), Some("abc")) //Some((42, "abc"))
    Semigroupal[Option].product(Some(42), None) //None
    Semigroupal[Future].product(Future(42), Future("abc"))

    Semigroupal[List].product(List(1, 2), List("a", "b")) //List((1,a), (1,b), (2,a), (2,b))
  }

  def productExercise(): Unit = {
    import cats.Monad
    import cats.syntax.functor._
    import cats.syntax.flatMap._

    def productWithMonads[F[_] : Monad, A, B](fa: F[A], fb: F[B]): F[(A, B)] =
      for {
        a <- fa
        b <- fb
      } yield (a, b)

    println(productWithMonads(List(1, 2), List("a", "b")))
  }

  def validated(): Unit = {
    import cats.data.Validated
    type ErrorsOr[T] = Validated[List[String], T]
    val v = Semigroupal[ErrorsOr].product(Validated.invalid(List("Nok", "Second Nok")), Validated.invalid(List("Another error")))
    println(v) //Invalid(List(Nok, Second Nok, Another error)) : 3-elements list for Validated, but not for monads ex. Either!

    type EitherErrorsOr[T] = Either[List[String], T]
    val e = Semigroupal[EitherErrorsOr].product(Left(List("Nok", "Second Nok")), Left(List("Another error")))
    println(e) //Left(List(Nok, Second Nok)) : 2-elements list
  }

  def zipSemigroupal(): Unit = {
    val zipListSemigroupal: Semigroupal[List] = new Semigroupal[List] {
      override def product[A, B](fa: List[A], fb: List[B]): List[(A, B)] = fa.zip(fb)
    }

    val s = zipListSemigroupal.product(List(1, 2, 3), List("a", "b"))
    println(s)
  }

}
