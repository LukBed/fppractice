package pl.snipersoft.advanced.darksyntaxsugar

import scala.util.Try

object MethodWithSingleParam extends App {
  def myMethod(arg: Int): String = s"Arg: $arg"
  val x = myMethod{42}

  val myTry = Try { throw new RuntimeException }

  List(1,2,3).map{_+1}
}