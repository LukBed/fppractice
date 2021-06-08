package pl.snipersoft.cats.typeclasses

import cats.data.Validated
import cats.{Applicative, Foldable, Functor, Id, Monad, Traverse}

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object Traversing extends App {
  implicit val ex: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

  introduction()
  traverseExercise()
  sequenceExercise()
  filterExercise()
  validatedExercise()
  futureExample()

  trait MyTraverse[L[_]] extends Foldable[L] with Functor[L] {
    def traverse[F[_] : Applicative, A, B ](container: L[A])(func: A => F[B]): F[L[B]]
    def sequence[F[_] : Applicative, A](container: L[F[A]]): F[L[A]] = traverse(container)(identity)

    def map[A, B](wa: L[A])(f: A => B): L[B] = traverse[Id, A, B](wa)(f) //from Functor
  }

  //List[String] => List[Future[Int]] => Future[List[Int]]
  def introduction() {
    val servers: List[String] = List("wwairsoft.net", "wwtanks.net", "wwwarships.net")

    def getBandWith(hostName: String): Future[Int] = Future(hostName.length * 80)

    val allBandwidthsManual: Future[List[Int]] = {
      servers.foldLeft(Future(List.empty[Int]))((currentFuture, hostName) => for {
        currentValue <- currentFuture
        newRecord <- getBandWith(hostName)
      } yield currentValue :+ newRecord)
    }

    val allBandwidthsTravers: Future[List[Int]] = Future.traverse(servers)(getBandWith)
    val allBandwidthsSequence: Future[List[Int]] = Future.sequence(servers.map(getBandWith)) //List[Future] => Future[List]

    allBandwidthsManual.foreach(println)
    allBandwidthsTravers.foreach(println) //one-liner based on foldLeft also!
    allBandwidthsSequence.foreach(println) //one-liner based on foldLeft also!
  }

  def traverseExercise(): Unit = {
    import cats.syntax.applicative._ //pure
    import cats.syntax.flatMap._ //flatMap
    import cats.syntax.functor._ //map
    import cats.syntax.apply._ //mapN

    def listTraverseMonad[F[_] : Monad, A, B](list: List[A])(func: A => F[B]): F[List[B]] =
      list.foldLeft(List.empty[B].pure[F])((currentWrapped, newArgument) =>
        for {
          current <- currentWrapped
          newWrapped <- func(newArgument)
        } yield current :+ newWrapped)

    def listTraverseApplicative[F[_] : Applicative, A, B](list: List[A])(func: A => F[B]): F[List[B]] =
      list.foldLeft(List.empty[B].pure[F])((currentWrapped, newArgument) =>
        (currentWrapped, func(newArgument)).mapN((a, b) => a :+ b))

    val traversedMonad = listTraverseMonad(List(1, 2, 3))(n => Option(n).map(_.toString + "a")) //Some(List(1a, 2a, 3a))
    val traversedApplicative = listTraverseApplicative(List(1, 2, 3))(n => Option(n).map(_.toString + "a")) //without monad!
    println(traversedMonad)
    println(traversedApplicative)
  }

  def sequenceExercise(): Unit = {
    import cats.syntax.applicative._ //pure
    import cats.syntax.apply._ //mapN

    //ex. //List[Future] => Future[List]
    def listSequence[F[_] : Applicative, A](list: List[F[A]]): F[List[A]] =
      list.foldLeft(List.empty[A].pure[F])((currentWrapped, newValue) => (currentWrapped, newValue).mapN((a, b) => a :+ b))
    //      listTraverseApplicative(list)(identity)

    val futures: List[Future[Int]] = List(Future(1), Future(2), Future(3))
    listSequence(futures).foreach(println) //Future(List(1,2,3))

    println(listSequence(List(Vector(1, 2), Vector(3, 4)))) //all possible 2-pairs - Vector(List(1, 3), List(1, 4), List(2, 3), List(2, 4)) -
    println(listSequence(List(Vector(1, 2), Vector(3, 4), Vector(5, 6)))) //all possible 3-pairs - Vector(List(1, 3, 5), List(1, 3, 6), List(1, 4, 5), List(1, 4, 6), List(2, 3, 5), List(2, 3, 6), List(2, 4, 5), List(2, 4, 6))
  }

  def filterExercise(): Unit = {
    def filterAsOption(list: List[Int])(predicate: Int => Boolean): Option[List[Int]] =
      Traverse[List].traverse(list)(n => Some(n).filter(predicate))

    val allTrue = filterAsOption(List(2, 4, 6))(_ % 2 == 0) //Some(List(2,4,6))
    val someFalse = filterAsOption(List(1, 2, 3))(_ % 2 == 0) //None
    println(allTrue)
    println(someFalse)
  }

  def validatedExercise(): Unit = {
    type ErrorsOr[T] = Validated[List[String], T]

    def filterAsValidated(list: List[Int])(predicated: Int => Boolean): ErrorsOr[List[Int]] =
      Traverse[List].traverse(list)(n => if (predicated(n)) Validated.valid(n) else Validated.invalid(List(s" $n failed")))

    val allTrue = filterAsValidated(List(2, 4, 6))(_ % 2 == 0) //Valid(List(2,4,6))
    val someFalse = filterAsValidated(List(1, 2, 3))(_ % 2 == 0) //all errors - Invalid(List(1 failed, 3 failed))
    println(allTrue)
    println(someFalse)
  }

  def futureExample(): Unit = {
    import cats.syntax.traverse._
    val servers: List[String] = List("wwairsoft.net", "wwtanks.net", "wwwarships.net")
    def getBandWith(hostName: String): Future[Int] = Future(hostName.length * 80)
    val f1 = Traverse[List].traverse(servers)(getBandWith)
    val f2 = servers.traverse(getBandWith)
    f1.foreach(println)
    f2.foreach(println)
  }
}
