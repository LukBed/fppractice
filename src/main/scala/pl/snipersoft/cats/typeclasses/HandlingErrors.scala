package pl.snipersoft.cats.typeclasses

import cats.{Applicative, ApplicativeError, Monad, MonadError}
import cats.instances.either._
import cats.instances.try_._
import cats.instances.future._
import cats.data.Validated
import cats.syntax.applicative._



import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

object HandlingErrors extends App {

  trait MyApplicativeError[M[_], E] extends Applicative[M] {
    def raiseError[A](e: E): M[A]
    def handleErrorWith[A](ma: M[A])(f: E => M[A]): M[A]
    def handleError[A](ma: M[A])(f: E => A): M[A] = handleErrorWith(ma)(e => pure(f(e)))
   }

  trait MyMonadError[M[_], E] extends MyApplicativeError[M, E] with Monad[M] {
    def ensure[A](ma: M[A])(error: E)(predicate: E => Boolean): M[A]
  }

  type ErrorOr[A] = Either[String, A]
  val me = MonadError[ErrorOr, String]
  val success = me.pure(32) //Either[String, Int] == Right(32)
  val failure = me.raiseError[Int]("nok") //Either[String, Int] == Left("nok")
  val okFromNok: ErrorOr[Int] = me.handleError(failure) { e => 42 } //error -> correct value, "recover"
  val handleErrorWith: ErrorOr[Int] = me.handleErrorWith(failure)(e => me.pure(23)) //error -> wrapped, "recoverWith"
  val filtered = me.ensure(success)("Number too small")(_ > 100) //if not return error, "filter"

  val ex = new RuntimeException("Nok")
  val pureException: Try[Int] = MonadError[Try, Throwable].raiseError(ex)

  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(10))
  val f: Future[Int] = MonadError[Future, Throwable].raiseError(ex) //Failure(exception)

  type ErrorsOr[T] = Validated[List[String], T]
  val a = ApplicativeError[ErrorsOr, List[String]].raiseError(List("Nok"))

  //extension methods
  import cats.syntax.applicativeError._
  val ok: ErrorsOr[Int] = 42.pure[ErrorsOr]
  val nok: ErrorsOr[Int] = List("nok").raiseError[ErrorsOr, Int]
  val recovered: ErrorsOr[Int] = nok.recover { case _ => 43 }

  import cats.syntax.monadError._
  val ensured: ErrorsOr[Int] = ok.ensure(List("nok"))(_>100)
}
