package pl.snipersoft.advanced.implicits

object ImplicitMethods extends App {
  case class Person(name: String) {
    def greet = s"Hi, my name is $name"
  }

  implicit def personFromString(s: String): Person = Person(s)

  val p: Person = "Kurt"
  println("Janek".greet)

  //if it's more than one possible solution, code will not compile!
  /*
  class A {
    def greet: Int = 2
  }
  implicit def aFromString(s: String): A = new A
  */
}
