package pl.snipersoft.advanced.patternmatching

object ListPattern extends App {
  val myList = List(1)
  myList match {
    case head :: Nil => println(s"The only element is $head")
    case Nil =>
  }
}