package pl.snipersoft.advanced

object ListPattern extends App {
  val myList = List(1)
  myList match {
    case head :: Nil => println(s"The only element is $head")
    case Nil =>
  }
}

object StandardClassInsteadOfCaseClass extends App {

  class PersonClass(val name: String, val age: Int)

  object Person {
    def unapply(person: PersonClass): Option[(String, Int)] =
      if (person.age < 21) None
      else Some((person.name, person.age))

    def unapply(age: Int): Option[String] = Some(if (age < 21) "minor" else "major")
  }

  val marysia = new PersonClass("Marysia", 27)
  //  val marysia = new PersonClass("Marysia", 20) //age 20 won't be matched
  val greeting = marysia match {
    case Person(n, a) => s"Hi, my name is $n and I am $a years old"
  }

  val legalStatus = marysia.age match {
    case Person(status) => s"My legal status is $status"
  }

  println(greeting)
  println(legalStatus)
}

object ReusePatternMatchingCondition extends App {

  object even {
    def unapply(arg: Int): Boolean = arg % 2 == 0
  }

  object singleDigit {
    def unapply(arg: Int): Boolean = arg < 10 && arg > -10
  }

  def matchNumber(n: Int): String = n match {
      case singleDigit() => "single digit"
      case even() => "an even number"
      case _ => "no property"
    }

  println(matchNumber(12))
  println(matchNumber(9))
  println(matchNumber(21))
}

object InfixPattern extends App {
  case class Or[A, B](a: A, b: B) //Either

  val either = Or(2, "two")

  val humanDescription = either match {
//    case Or(number, text) => s"$number is written as $text"
    case number Or text => s"$number is written as $text" //the same pattern
  }

  println(humanDescription)
}

object DecomposingSequences extends App { //var args pattern
//  val vararg = List(1) match {
//    case List(1, _*) => "starting with one"
//  }

  abstract class MyList[+A] {
    def head: A = ???
    def tail: MyList[A] = ???
  }
  case object Empty extends MyList[Nothing]
  case class Cons[+A](override val head: A, override val tail: MyList[A]) extends MyList[A]

  object MyList {
    def unapplySeq[A](list: MyList[A]): Option[Seq[A]] =
      if (list == Empty) Some(Seq.empty)
      else unapplySeq(list.tail).map(list.head +: _) //+: is :: for sequence
  }

  val myList: MyList[Int] = Cons(1, Cons(2, Cons(3, Empty)))
  val decomposed = myList match {
    case MyList(1, 2, _*) => "starting with 1, 2" //if _* check unapplySeq
    case _ => "starting with something else"
  }

  println(decomposed)
}

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