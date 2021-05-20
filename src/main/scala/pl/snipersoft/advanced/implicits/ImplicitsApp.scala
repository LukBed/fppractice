package pl.snipersoft.advanced.implicits

object ImplicitsApp extends App {

  implicitClass()
  implicitMethod()
  implicitParameters()
  exerciseOrdering()
  organizing()
  exercisePurchases()

  def implicitClass(): Unit = {
    val pair = "Tiger" -> 88

    implicit class RichString(s: String) {
      def twice: String = s + s
    }

    println("abc".twice)
  }

  def implicitMethod(): Unit = {
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

  def implicitParameters(): Unit = {
    def increment(n: Int)(implicit amount: Int): Int = n + amount
    implicit val defaultAmount: Int = 10
    println(increment(5))

//    implicit val myOrdering: Ordering[Int] = Ordering.fromLessThan(_>_) //ok
//    implicit def myOrdering: Ordering[Int] = Ordering.fromLessThan(_>_) //ok
//    implicit def myOrdering(): Ordering[Int] = Ordering.fromLessThan(_>_) //nok

    println(List(1,5,3,2,4).sorted) //from scala.Predef

    /*
    Implicit values:
    -val/var
    -objets
    -accessor methods = defs without ()
     */

    /*
    Scopes:
    -local
    -imported
    -companion objects of all types involved in the method signature
     def sorted[B >: A](implicit ord: Ordering[B]): List[B]
     List, Ordering, all the types involved (A or any supertype)
     */
  }

  def exerciseOrdering(): Unit = {
    case class Person(name: String, age: Int)

    implicit val personsOrdering: Ordering[Person] = Ordering.fromLessThan((p1, p2) => p1.name.compareTo(p2.name)<0)

    val persons = List(Person("Janek", 15), Person("Zenek", 20), Person("Anastazja", 100))
    println(persons.sorted)
  }

  def organizing(): Unit = {
    case class Person(name: String, age: Int)
    val persons = List(Person("Janek", 15), Person("Zenek", 20), Person("Anastazja", 100))

    object AlphabeticOrdering {
      implicit val alphabeticOrdering: Ordering[Person] = Ordering.fromLessThan((p1, p2) => p1.name.compareTo(p2.name)<0)
    }

    object AgeOrdering {
      implicit val alphabeticOrdering: Ordering[Person] = Ordering.fromLessThan((p1, p2) => p1.name.compareTo(p2.name)<0)
    }

    import AgeOrdering._
    println(persons.sorted)
  }

  def exercisePurchases(): Unit = {
    //good practices

    //the most often case in companion object
    object Purchase {
      implicit val purchasesOrdering: Ordering[Purchase] = Ordering.fromLessThan((p1, p2) => p1.nUnits*p1.unitPrice < p2.nUnits*p2.unitPrice)
    }

    //other cases in another objects
    object UnitCountOrdering {
      implicit val unitCountOrdering: Ordering[Purchase] = Ordering.fromLessThan(_.nUnits<_.nUnits)
    }

    object UnitPriceOrdering {
      implicit val unitPriceOrdering: Ordering[Purchase] = Ordering.fromLessThan(_.unitPrice<_.unitPrice)
    }

    case class Purchase(nUnits: Int, unitPrice: Double)

    val purchases = List(Purchase(100, 2), Purchase(20, 3), Purchase(1000, 0.001))
    println(purchases.sorted)

    import UnitCountOrdering._
    println(purchases.sorted)
  }
}
