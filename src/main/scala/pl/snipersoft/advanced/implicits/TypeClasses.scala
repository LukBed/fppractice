package pl.snipersoft.advanced.implicits

object TypeClasses extends App {
  intro()
  exerciseEqual()

  case class User(name: String, age: Int, mail: String)

  def intro(): Unit = {

    val janek = User("Janek", 25, "janek@jankowo.pl")

    //type class
    trait HTMLSerializer[T] {
      def serialize(value: T): String
    }

    object HTMLSerializer {
      def serialize[T](value: T)(implicit serializer: HTMLSerializer[T]): String = serializer.serialize(value)
      def apply[T](implicit serializer: HTMLSerializer[T]): HTMLSerializer[T] = serializer
    }

    //type class instance
    implicit object UserHTMLSerializer extends HTMLSerializer[User] {
      override def serialize(u: User): String = s"<div>${u.name}, ${u.age} (<i>${u.mail}</i>)</div>"
    }

    implicit object IntSerializer extends HTMLSerializer[Int] {
      override def serialize(value: Int): String = s"<div><i>$value</i></div>"
    }

    println(HTMLSerializer.serialize(janek))
    println(HTMLSerializer.serialize(45))

    //access to entire type class methods
    println(HTMLSerializer[User].serialize(janek))
  }

  def exerciseEqual(): Unit = {
    trait Equal[T] {
      def isEqual(o1: T, o2: T): Boolean
    }

    object Equal {
      def apply[T](implicit equal: Equal[T]): Equal[T] = equal
      def apply[T](o1: T, o2: T)(implicit equalizer: Equal[T]): Boolean = equalizer.isEqual(o1, o2)
    }

    implicit object UsersNameEqual extends Equal[User] {
      override def isEqual(o1: User, o2: User): Boolean = o1.name == o2.name
    }

    object UsersNameAndAgeEqual extends Equal[User] {
      override def isEqual(o1: User, o2: User): Boolean = o1.name == o2.name && o1.age == o2.age
    }

    val u1 = User("Janek", 25, "janek@janek.pl")
    val u2 = User("Marek", 25, "marek@janek.pl")
    println(Equal[User].isEqual(u1, u2))

    //ad-hoc polymorphism
    println(Equal.apply(u1, u2))
  }



}
