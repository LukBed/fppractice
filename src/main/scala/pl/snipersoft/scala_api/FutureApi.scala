package pl.snipersoft.scala_api

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object FutureApi extends App {
  val future1 = Future.successful("a")
  val future2 = Future.successful(2)
  val future3 = Future.successful(true)
  val fail = Future.failed(new RuntimeException("Ex"))

  future1.zip(future2).print("OK-OK") //(a, 2)
  future1.zip(fail).print("OK-NOK")
  fail.zip(future1).print("NOK-OK")

  future1.zipWith(future2) { (a, b) => a + b.toString }.print("Zip with")


  implicit class MyFuture[T](future: Future[T]) {
    def print[S](s: S): Unit = future.onComplete(v => println(s"$s: " + v))
  }
}
