package pl.snipersoft.advanced.implicits.typeclasses

object EqualityPlayground extends App {

  case class User(name: String, age: Int, mail: String)

  trait Equal[T] {
    def isEqual(o1: T, o2: T): Boolean
  }

  object Equal {
    def apply[T](implicit equal: Equal[T]): Equal[T] = equal

    def apply[T](o1: T, o2: T)(implicit equalizer: Equal[T]): Boolean = equalizer.isEqual(o1, o2)
  }

  implicit object UsersNameEqualizer extends Equal[User] {
    override def isEqual(o1: User, o2: User): Boolean = o1.name == o2.name
  }

  object FullUserEqualizer extends Equal[User] {
    override def isEqual(o1: User, o2: User): Boolean = o1.name == o2.name && o1.mail == o2.mail
  }

  val janek = User("Janek", 25, "janek@janek.pl")
  val janek2 = User("Janek", 30, "janek2@janek.pl")

  //ad-hoc polymorphism
//  println(Equal.apply(janek, janek2))

  implicit class EqualityEnrichment[T](val o1: T) extends AnyVal {
    def ===(o2: T)(implicit equalizer: Equal[T]): Boolean = {
      println("Sprawdzam równość")
      equalizer.isEqual(o1, o2)
    }
    def !==(o2: T)(implicit equalizer: Equal[T]): Boolean = {
      println("Sprawdzam nierówność")
      !equalizer.isEqual(o1, o2)
    }
  }

  println(janek === janek2)
  // new EqualityEnrichment[User](janek).===(janek2)(UsersNameEqualizer)
  println(janek !== janek2)

  println(janek.===(janek2)(FullUserEqualizer))

}
