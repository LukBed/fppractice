package pl.snipersoft.advanced.typesystem

object HigherKindedTypes extends App {

//  hkt()
  implicitHkt()

  def hkt(): Unit = {
    trait Monad[F[_], A] {
      def flatMap[B](f: A => F[B]): F[B]
      def map[B](f: A => B): F[B]
    }

    class MonadList[A](list: List[A]) extends Monad[List, A] {
      override def flatMap[B](f: A => List[B]): List[B] = list.flatMap(f)
      override def map[B](f: A => B): List[B] = list.map(f)
    }

    class MonadOption[A](option: Option[A]) extends Monad[Option, A] {
      override def flatMap[B](f: A => Option[B]): Option[B] = option.flatMap(f)
      override def map[B](f: A => B): Option[B] = option.map(f)
    }

    def multiple[F[_], A, B](mA: Monad[F, A], mB: Monad[F, B]): F[(A, B)] =
      for {
        a <- mA
        b <- mB
      } yield (a, b)

    println(multiple(new MonadList(List(1, 2)), new MonadList(List("a", "b"))))
    println(multiple(new MonadOption(Some(1)), new MonadOption(Some("a"))))
  }

  def implicitHkt(): Unit = {
    trait Monad[F[_], A] { //higher-kinded type class
      def flatMap[B](f: A => F[B]): F[B]
      def map[B](f: A => B): F[B]
    }

    implicit class MonadList[A](list: List[A]) extends Monad[List, A] {
      override def flatMap[B](f: A => List[B]): List[B] = list.flatMap(f)
      override def map[B](f: A => B): List[B] = list.map(f)
    }

    implicit class MonadOption[A](option: Option[A]) extends Monad[Option, A] {
      override def flatMap[B](f: A => Option[B]): Option[B] = option.flatMap(f)
      override def map[B](f: A => B): Option[B] = option.map(f)
    }

    implicit def multiple[F[_], A, B](implicit mA: Monad[F, A], mB: Monad[F, B]): F[(A, B)] =
      for {
        a <- mA
        b <- mB
      } yield (a, b)

    println(multiple(List(1, 2), List("a", "b")))
    println(multiple(Some(1), Some("a")))
  }

}
