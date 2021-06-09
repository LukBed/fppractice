package pl.snipersoft.cats.alien

import cats.Monoid

//given Format[MyType], can we have also a Format[Option[MyType]]?
object ContravariantFunctorsAndContramap extends App {

  //contravariant type class (not connected with contravariant generics!)
  trait Format[T] { self => //alias to this
    def format(value: T): String

    def contramap[A](func: A => T): Format[A] = new Format[A] {
      override def format(value: A): String = self.format(func(value))
    }
  }

  def format[T](value: T)(implicit f: Format[T]): String = f.format(value)

  implicit object StringFormat extends Format[String] {
    override def format(value: String): String = "\"" + value + "\""
  }

  implicit object IntFormat extends Format[Int] {
    override def format(value: Int): String = value.toString
  }

  implicit object BooleanFormat extends Format[Boolean] {
    override def format(value: Boolean): String = if (value) "T" else "F"
  }

  implicit def getOptionFormat[T](implicit f: Format[T], m: Monoid[T]): Format[Option[T]] =
//    (value: Option[T]) => f.format(value.get)
//    contramap[Option[T], T](_.get)
    f.contramap[Option[T]](_.getOrElse(m.empty))

//  def contramap[A, B](func: A => B)(implicit f: Format[B]): Format[A] =
//    (value: A) => f.format(func(value))

  println(format("abc"))
  println(format(42))
  println(format(true))
  println(format(Option(42)))
  println(format[Option[Int]](None))
  println(format(Option(Option(42))))

  /*
  IntFormat
  fo: Format[Option[Int]] = IntFormat.contramap[Option[Int]](_.get) //first get
  fo2: Format[Option[Option[Int]]] = IntFormat.contramap[Option[Option[Int]]](_.get) //second get
    = IntFormat.contramap[Option[Int]](_.get) //first get
      .contramap[OptionOption[[Int]]](_.get) //second get

   fo2.format(Option(Option(42)) =
     fo1.format(secondGet(option(Option(42))) =
     IntFormat.format(firstGet(secondGet(Option(Option(42))))

   order is REVERSE from the written order - stack order!
   second get -> first get -> format of Int
   no as map - map applies transformations in sequence
   so it's called contravariant type class
   */

  import cats.Contravariant
  import cats.Show
  import cats.instances.int._
  val showOption: Show[Option[Int]] = Contravariant[Show].contramap(Show[Int])(_.getOrElse(0))
  println(showOption.show(Some(25)))

  import cats.syntax.contravariant._
  val showOptionShorter: Show[Option[Int]] = Show[Int].contramap(_.getOrElse(0))
  println(showOptionShorter.show(Some(20)))
}
