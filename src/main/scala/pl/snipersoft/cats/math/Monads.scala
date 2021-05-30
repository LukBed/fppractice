package pl.snipersoft.cats.math

import cats.Monad
import cats.implicits._
import cats.instances.option._
import cats.instances.list._
import cats.instances.future._
import cats.instances.either._
import cats.syntax.monad._

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

//HKT, Monad extends Functor, Monad = Functor(map) + pure + flatMap
object Monads extends App {

  myMonad()
  catsMonad()
  pairsExercise()
  extensionMethods()
  either()
  generalizationExercise()
  customMonads()
  customIdentityMonad()
  treeMonadStackRecursive()
  treeMonadTailRecursive()

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

  def either(): Unit = {
    type LoadingOr[T] = Either[String, T]
    val either: LoadingOr[Int] = Monad[LoadingOr].pure(25) //Right(25)
  }

  def generalizationExercise(): Unit = {
    case class Connection(host: String, port: String)
    type HttpConfiguration = Map[String, String]
    type LoadingOr[T] = Either[String, T]

    val config: HttpConfiguration = Map("host" -> "localhost", "port" -> "4040")
    val payload = "abcdefgh"

    trait HttpService[M[_]] {
      def getConnection(config: HttpConfiguration): M[Connection]
      def issueRequest(connection: Connection, payload: String): M[String]
    }

    object HttpService {
      def apply[T[_]](implicit service: HttpService[T]): HttpService[T] = service
    }

    abstract class AbstractHttpService[M[_]](implicit monad: Monad[M]) extends HttpService[M] {
      override def getConnection(config: HttpConfiguration): M[Connection] = {
        val optionConnection = for {
          host <- config.get("host")
          port <- config.get("port")
        } yield Connection(host, port)
        optionConnection match {
          case Some(c) => monad.pure(c)
          case None => nok("Wrong configuration")
        }
      }

      override def issueRequest(connection: Connection, payload: String): M[String] =
        if (payload.length > 5) monad.pure("Request has been accepted")
        else nok("Request has not been accepted")

      protected def nok[T](msg: String): M[T]
    }

    implicit object TryHttpService extends AbstractHttpService[Try] {
      override protected def nok[T](msg: String): Try[T] = Failure(new IllegalArgumentException(msg))
    }

    implicit object OptionHttpService extends AbstractHttpService[Option] {
      override protected def nok[T](msg: String): Option[T] = None
    }

    implicit object FutureHttpService extends AbstractHttpService[Future] {
      override protected def nok[T](msg: String): Future[T] = Future.failed(new IllegalArgumentException(msg))
    }

    implicit object EitherHttpService extends AbstractHttpService[LoadingOr] {
      override protected def nok[T](msg: String): LoadingOr[T] = Left(msg)
    }

    def testService[T[_] : Monad](implicit service: HttpService[T]): Unit = {
      def executeTestCase(conf: HttpConfiguration, pl: String): Unit = println( for {
        connection <- service.getConnection(conf)
        response <- service.issueRequest(connection, pl)
      } yield response)

      executeTestCase(config, payload)
      executeTestCase(config, "")
      executeTestCase(Map(), payload)
      println()
    }

    testService[Try]
    testService[Option]
    testService[Future]
    testService[LoadingOr]
  }

  def customMonads(): Unit = {
    implicit object OptionMonad extends Monad[Option] {
      override def pure[A](x: A): Option[A] = Option(x)
      override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
      //for iteration methods
      @tailrec
      override def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] =
        f(a) match {
          case None => None
          case Some(Left(v)) => tailRecM(v)(f) //if left run this method again with new argument
          case Some(Right(v)) => Some(v)
        }
    }
  }

  def customIdentityMonad(): Unit = {
    type Identity[T] = T
    val n: Identity[Int] = 42

    implicit object IdentityMonad extends Monad[Identity] {
      override def pure[A](x: A): Identity[A] = x
      override def flatMap[A, B](fa: Identity[A])(f: A => Identity[B]): Identity[B] = f(fa)
      @tailrec
      override def tailRecM[A, B](a: A)(f: A => Identity[Either[A, B]]): Identity[B] = f(a) match {
        case Left(v) => tailRecM(v)(f)
        case Right(v) => v
      }
    }

    println(n.flatMap(x => x+10))
  }

  def treeMonadStackRecursive(): Unit = {
    sealed trait Tree[+A]
    final case class Leaf[+A](a: A) extends Tree[A]
    final case class Branch[+A](left: Tree[A], right: Tree[A]) extends Tree[A]

    implicit object TreeMonad extends Monad[Tree] {
      override def pure[A](x: A): Tree[A] = Leaf(x)
      override def flatMap[A, B](a: Tree[A])(f: A => Tree[B]): Tree[B] = a match {
        case Leaf(v) => f(v)
        case Branch(l, r) => Branch(flatMap(l)(f), flatMap(r)(f))
      }
      override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = {
        def stackRec(t: Tree[Either[A, B]]): Tree[B] = t match {
          case Leaf(Left(v)) => stackRec(f(v))
          case Leaf(Right(v)) => Leaf(v)
          case Branch(l, r) => Branch(stackRec(l), stackRec(r))
        }
        stackRec(f(a))
      }
    }

    val tree: Tree[Int] = Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))
    val newTree = TreeMonad.flatMap(tree)(v => Branch(Leaf(v), Leaf(10*v)))
    println(newTree)
  }

  def treeMonadTailRecursive(): Unit = {
    sealed trait Tree[+A]
    final case class Leaf[+A](a: A) extends Tree[A]
    final case class Branch[+A](left: Tree[A], right: Tree[A]) extends Tree[A]

    implicit object TreeMonad extends Monad[Tree] {
      override def pure[A](x: A): Tree[A] = Leaf(x)
      override def flatMap[A, B](a: Tree[A])(f: A => Tree[B]): Tree[B] = a match {
        case Leaf(v) => f(v)
        case Branch(l, r) => Branch(flatMap(l)(f), flatMap(r)(f))
      }
      override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = {
        @tailrec
        def tailRec(todo: List[Tree[Either[A, B]]], expanded: Set[Tree[Either[A, B]]],
                    done: List[Tree[B]]): Tree[B] = {
          if (todo.isEmpty) done.head
          else todo.head match {
            case Leaf(Left(v)) => tailRec(f(v) :: todo.tail, expanded, done)
            case Leaf(Right(b)) => tailRec(todo.tail, expanded, Leaf(b) :: done)
            case node @ Branch(left, right) =>
              if (!expanded.contains(node)) tailRec(right :: left :: todo, expanded + node, done)
              else {
                val newLeft = done.head
                val newRight = done.tail.head
                val newBranch = Branch(newLeft, newRight)
                tailRec(todo.tail, expanded, newBranch :: done.drop(2))
              }
          }

        }

        tailRec(List(f(a)), Set(), List())
      }
    }

    val tree: Tree[Int] = Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))
    val newTree = TreeMonad.flatMap(tree)(v => Branch(Leaf(v), Leaf(10*v)))
    println(newTree)
  }
}
