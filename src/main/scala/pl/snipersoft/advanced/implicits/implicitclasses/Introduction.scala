package pl.snipersoft.advanced.implicits.implicitclasses

object Introduction extends App {

  implicit class RichString(val s: String) extends AnyVal { //extends AnyVal for memory optimization
    def twice(): String = s + s
  }

  //compilator doesn't do multiple implicit searches for implicit classes
  implicit class RicherString(val s: String) extends AnyVal {
    def doIt(): String = "hello " + s
  }

  //type enrichment - pimping
  println("abc".twice()) //new RichString("abc").twice
  1 to 10
  "Tiger" -> 88
  import scala.concurrent.duration._
  3.seconds

  //    implicit class RicherStringWrapper(val s: RichString) extends AnyVal {
  //      def doSomething(): String = "hello " + s.s
  //    }

  "abc".twice()
  "abc".doIt()
  //"abc".doSomething() //not compiling
}