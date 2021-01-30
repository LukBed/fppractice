package pl.snipersoft.advanced.darksyntaxsugar

object UpdateMethod extends App { //used in mutable collections
  val anArray = Array(1,2,3)
  anArray(2) = 7 // anArray.update(2,7)
}