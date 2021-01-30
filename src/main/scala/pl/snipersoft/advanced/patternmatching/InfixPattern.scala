package pl.snipersoft.advanced.patternmatching

object InfixPattern extends App {
  case class Or[A, B](a: A, b: B) //Either

  val either = Or(2, "two")

  val humanDescription = either match {
//    case Or(number, text) => s"$number is written as $text"
    case number Or text => s"$number is written as $text" //the same pattern
  }

  println(humanDescription)
}