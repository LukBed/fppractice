package pl.snipersoft.cats.alien

import cats.data.Kleisli

//function composition for wrapped values
object Kleislis extends App {

  val fp1: Int => String = n => if (n%2 == 0) "ok" else "nok"
  val fp2: Int => Int = _+1
  val fp3 = fp2 andThen f1

  val f1: Int => Option[String] = x => Option(x).filter(_%2 == 0).map(_.toString)
  val f2: Int => Option[Int] = x => Option(x).map(_*3)
//  val f3 = f2 andThen f1 //not compile

  //Kleisli is a case class
  import cats.instances.option._ //FlatMap[Option]
  val fk1: Kleisli[Option, Int, String] = Kleisli(f1)
  val fk2: Kleisli[Option, Int, Int] = Kleisli(f2)
  val fk3: Kleisli[Option, Int, String] = fk2 andThen fk1

  fk2.map(_*2).flatMap(n => Kleisli(f1))

  import cats.Id
  type InterestingKleisli[A, B] = Kleisli[Id, A, B] //Reader[A, B] is the Kleisli[Id, A, B]
}
