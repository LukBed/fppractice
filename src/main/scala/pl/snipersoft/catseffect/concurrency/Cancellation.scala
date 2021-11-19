package pl.snipersoft.catseffect.concurrency

import cats.effect.{IO, IOApp}
import pl.snipersoft.catseffect.utils.IoOps

import scala.concurrent.duration.DurationInt

object Cancellation extends IOApp.Simple {

  /*
  Canceling IOs:
  - fib.cancel
  - IO.race & other APIs
  - manual cancellation IO.canceled
   */

  val paymentSystem: IO[String] = (IO("Payment started").debug >>
    IO.sleep(1.second) >>
    IO("Payment completed").debug).onCancel(IO("We're doomed!").debug.void)

  val cancellation: IO[Unit] = for {
    fib <- paymentSystem.start
    _ <- IO.sleep(500.millis) >> IO("Canceling").debug >> fib.cancel
  } yield ()

  val atomicPayment: IO[String] = IO.uncancelable(_ => paymentSystem) //masking
  val atomicPaymentV2: IO[String] = paymentSystem.uncancelable

  val noCancellation: IO[Unit] = for {
    fib <- atomicPayment.start
    _ <- IO.sleep(500.millis) >> IO("Canceling").debug >> fib.cancel
  } yield ()

  override def run: IO[Unit] = noCancellation
}

object AuthenticationApp extends IOApp.Simple {
  val inputPassword: IO[String] = IO("Input password:").debug >> IO("*****").debug >> IO.sleep(2.seconds) >> IO("MyPass").debug
  val verifyPassword: String => IO[Boolean] = (pass: String) => IO("Veryfing...").debug >> IO.sleep(2.seconds) >> IO(pass == "MyPass")
  val authFlow: IO[Unit] = IO.uncancelable { poll =>
    for {
      pass <- poll(inputPassword).onCancel(IO("Authentication failed. Try again later.").debug.void) //cancelable!
      verified <- verifyPassword(pass) //not cancelable
      _ <- if (verified) IO("Hello!").debug else IO("Wrong password!").debug //not cancelable
    } yield ()
  }

  val authProgram: IO[Unit] = for {
    authFib <- authFlow.start
    _ <- IO.sleep(1.second) >> authFib.cancel
  } yield ()

  val cancelableAuthFlow: IO[Unit] = IO.uncancelable { poll => poll(authFlow) } //every element is cancelable now!

  override def run: IO[Unit] = authProgram
}

object ThreeSteps extends IOApp.Simple {
  val program: IO[Unit] = IO.uncancelable { poll =>
    poll(IO("Cancelable").debug >> IO.sleep(1.second)) >>
      IO("Uncancelable").debug >> IO.sleep(1.second) >>
      poll(IO("Second cancelable").debug >> IO.sleep(1.second))
  }

  /**
   * 500 ms - cancelable => canceling
   * 1500 ms - cancelable => uncancelable => canceling - second cancelable will not be printed, cancelation effect will be delayed!!!
   * 2500 ms - cancelable => uncancelable => second cancelable => canceling
   */
  override def run: IO[Unit] = for {
    fib <- program.start
    _ <- IO.sleep(1500.millis) >> IO("Canceling...").debug >> fib.cancel
    _ <- fib.join
  } yield ()
}