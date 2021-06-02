package pl.snipersoft.cats.math

import cats.data.OptionT
import cats.data.EitherT
import cats.instances.list._
import cats.instances.future._
import cats.implicits._
import scala.concurrent.ExecutionContext.Implicits.global

import scala.concurrent.Future //OptionT[List]

object MonadTransformers extends App {

  optionTransformer()
  eitherTransformer()
  exercise()

  def optionTransformer() {
    val listOfNumbersOptions: OptionT[List, Int] = OptionT(List(1.some, 2.some)) //List[Option[Int]]
    val listOfCharactersOptions: OptionT[List, Char] = OptionT(List('a'.some, 'b'.some)) //List[Option[Char]]
    val listOfOptionTuples: OptionT[List, (Int, Char)] = for {
      n <- listOfNumbersOptions
      ch <- listOfCharactersOptions
    } yield (n, ch)
    val unwrapped: List[Option[(Int, Char)]] = listOfOptionTuples.value
    println(unwrapped)
  }

  def eitherTransformer(): Unit = {
    val futureTransformer: EitherT[Future, String, Int] = EitherT(Future(25.asRight))
    val anotherFutureTransformer: EitherT[Future, String, Int] = EitherT(Future(25.asRight))
    val modified: Future[Either[String, String]] = futureTransformer.map(x => (x+1).toString).value
    modified.foreach(println)
  }

  def exercise(): Unit = {
    type AsyncResponse[T] = EitherT[Future, String, T]
    val bandwidths = Map("server1" -> 50, "server2" -> 300, "server3" -> 170)

    def getBandWith(server: String): AsyncResponse[Int] = bandwidths.get(server) match {
      case None => EitherT[Future, String, Int](Future(Left(s"Server $server unreachable")))
      case Some(b) => EitherT[Future, String, Int](Future(Right(b)))
    }

    //capacity>250
    def canWithstandSurge(s1: String, s2: String): AsyncResponse[Boolean] = for {
        b1 <- getBandWith(s1)
        b2 <- getBandWith(s2)
      } yield (b1 + b2)>=250

    //canWithstandSurge == true -> Right, false -> Left: why not?
    def generateTrafficSpikeReport(s1: String, s2: String): AsyncResponse[String] = for {
      canWithstand <- canWithstandSurge(s1, s2)
      report <-
        if (canWithstand) EitherT[Future, String, String](Future(Right(s"Servers $s1 and $s2 can withstand surge")))
//        else EitherT[Future, String, String](Future(Left(s"Servers $s1 and $s2 can not withstand surge due to low capacity")))
        else EitherT[Future, String, String](Future(Left(s"Servers $s1 and $s2 can not withstand surge due to low capacity")))
    } yield report


    def test(s1: String, s2: String): Unit = generateTrafficSpikeReport(s1, s2).value.foreach(println)
    test("server1", "server2")
    test("server1", "server3")
    test("server1", "server4")
  }
}
