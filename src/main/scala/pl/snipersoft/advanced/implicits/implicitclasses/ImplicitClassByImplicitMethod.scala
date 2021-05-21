package pl.snipersoft.advanced.implicits.implicitclasses

object ImplicitClassByImplicitMethod extends App {
  //implementation of "implicit class" using implicit method

  class RichString(s: String)
  implicit def enrich(s: String): RichString = new RichString(s)

}
