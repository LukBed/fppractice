package pl.snipersoft.advanced.partialfunction

object PartialFunctionsInstance extends App {

  val aManualFussyFunction = new PartialFunction[Int, Int] {
    override def isDefinedAt(n: Int): Boolean = List(1, 3, 5).contains(n)

    override def apply(n: Int): Int = if (isDefinedAt(n)) n+1 else throw new RuntimeException
  }

  println(aManualFussyFunction(1))
  println(aManualFussyFunction(2))
}