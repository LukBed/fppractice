package pl.snipersoft.scala_api

object EitherApi extends App {

  import cats.implicits._

  val maybeError: Either[String, Int] = Left("2")
  val recovered: Int = maybeError.valueOr(_.toInt)
  println(recovered)
}
