package pl.snipersoft.advanced.currying

object Currying extends App {
  val curriedFunction: Int => Int => Int = x => y => x + y

  def curriedMethod(x: Int)(y: Int): Int = x + y

  //  val add4 = curriedMethod(4) // not compiling, because curried method is method
  //methods are part of classes and objects, not functions, must be transformed to functions (due to JVM limitations)
  //it's lifting or eta expansion
  val add4: Int => Int = curriedMethod(4)
  val add4_v2 = curriedMethod(4) _ //it works too

  def inc(x: Int) = x + 1

  List(1, 2, 3).map(inc) //compiler execute ETA-expansion (convert method into function)
}
