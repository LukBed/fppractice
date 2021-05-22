package pl.snipersoft.advanced.implicits

//avoid this kind of implicits - bugs is hard to find!
object ImplicitMethods extends App {

  introduction()
  danger()

  def introduction(): Unit = {
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

  def danger(): Unit = {
    implicit def isPositive(n: Int): Boolean = n>0
    //if (n) ...

    //it works, but our bug in implicit method is very hard to find
    if (25) println("Ok!")
    else if (-2) println("Nok")
  }

}
