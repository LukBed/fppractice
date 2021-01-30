package pl.snipersoft.advanced.patternmatching

object CustomReturnTypeForUnapply extends App {
  //isEmpty: Boolean, get: something

  class Person(val name: String, val age: Int)

  abstract class Wrapper[T] {
    def isEmpty: Boolean
    def get: T
  }

  object PersonWrapper {
    def unapply(person: Person): Wrapper[String] = new Wrapper[String] {
      override def isEmpty = false
      override def get = person.name
    }
  }

  val marysia = new Person("Marysia", 27)
  val matched = marysia match {
    case PersonWrapper(n) => s"This person's name is $n"
    case _ => "An alien"
  }

  println(matched)
}