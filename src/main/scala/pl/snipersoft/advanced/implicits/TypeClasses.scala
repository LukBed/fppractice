package pl.snipersoft.advanced.implicits

object TypeClasses extends App {
  intro()
  exercise()

  case class User(name: String, age: Int, mail: String)

  def intro(): Unit = {

    val janek = User("Janek", 25, "janek@jankowo.pl")

    //type class
    trait HTMLSerializer[T] {
      def serialize(value: T): String
    }

    //type class instance
    object UserHTMLSerializer extends HTMLSerializer[User] {
      override def serialize(u: User): String = s"<div>${u.name}, ${u.age} (<i>${u.mail}</i>)</div>"
    }

    println(UserHTMLSerializer.serialize(janek))
  }

  def exercise(): Unit = {
    trait Equal[T] {
      def apply(o1: T, o2: T): Boolean
    }

    object UsersNameEqual extends Equal[User] {
      override def apply(o1: User, o2: User): Boolean = o1.name == o2.name
    }

    object UsersNameAndAgeEqual extends Equal[User] {
      override def apply(o1: User, o2: User): Boolean = o1.name == o2.name && o1.age == o2.age
    }
  }



}
