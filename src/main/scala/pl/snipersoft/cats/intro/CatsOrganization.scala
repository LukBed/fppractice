package pl.snipersoft.cats.intro

object CatsOrganization extends App {

  import cats.Eq
  import cats.instances.int._ //import TC instances fot the types you need

  //TC API
  val intEquality = Eq[Int]
  println(intEquality.eqv(1, 2))
  //println(intEquality.eqv(1, "a")) //not compile

  //extension methods

  import cats.syntax.eq._

  2 eqv 3
  2 neqv 3
  2 === 3
  2 =!= 3
  //  2 === "s" //not compile

  //extending the TC operations to composite types ex. lists
  //import cats.instances.list._ //we bring Eq[List[Int]] in scope - for previous versions of Cats (probably)
  val listComparison = List(2) === List(3) //not compile in previous versions of Cats

  //create a TC instance for a custom type
  case class ToyCar(model: String, price: Double)

  implicit val toyCarEq: Eq[ToyCar] = Eq.instance(_.price == _.price)
  println(ToyCar("Car 1", 25) === ToyCar("Car 2", 25))

  //if have troubles
  //import cats._
  //import cats.implicits._
}
