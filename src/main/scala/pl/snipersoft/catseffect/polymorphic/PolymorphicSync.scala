package pl.snipersoft.catseffect.polymorphic

import cats.Defer
import cats.effect.{IO, IOApp, MonadCancel, Sync}

import java.io.{BufferedReader, InputStreamReader}

/** Synchronous computation. Wrapping computation into an effect. */
object PolymorphicSync extends IOApp.Simple {

  /*
  often better than use implicit Sync use just IO
  it can be also used with monad transformers
   */
  trait MySync[F[_]] extends MonadCancel[F, Throwable] with Defer[F] {
    def delay[A](thunk: => A): F[A] //thunk = computation, it's suspension of computation - will run on the CE thread pool
    def blocking[A](thunk: => A): F[A] //on some specific thread pool for blocking computations
    override def defer[A](thunk: => F[A]): F[A] = flatMap(delay(thunk))(identity)
  }

  trait Console[F[_]] {
    def println[A](a: A): F[Unit]
    def readLine(): F[String]
  }

  object Console {
    def apply[F[_]](implicit sync: Sync[F]): F[Console[F]] = {
      import cats.syntax.functor._
      sync.pure((System.in, System.out)).map {
        case (in, out) => new Console[F] {
          override def println[A](a: A): F[Unit] = sync.blocking(out.println(a))
          override def readLine(): F[String] = sync.interruptible(many = true)(new BufferedReader(new InputStreamReader(in)).readLine())
        }
      }
    }
  }

  def consoleReader(): IO[Unit] = for {
    console <- Console[IO]
    _ <- console.println("Hi, what's your name?")
    name <- console.readLine()
    _ <- console.println(s"Hello, $name")
  } yield ()

  override def run: IO[Unit] = consoleReader()
}
