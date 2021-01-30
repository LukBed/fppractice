package pl.snipersoft.advanced.patternmatching

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
