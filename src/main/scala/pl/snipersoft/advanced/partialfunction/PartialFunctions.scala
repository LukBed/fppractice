package pl.snipersoft.advanced.partialfunction

object PartialFunctions extends App {
  val aPartialFunction: PartialFunction[Int, Int] = { //pf extends normal function
    case 1 => 42
    case 2 => 56
    case 5 => 999
  } //partial function value

  println(aPartialFunction(1))
  //  println(aPartialFunction(10)) //throws exception
  println(aPartialFunction.isDefinedAt(5))

  println(aPartialFunction.lift(10))

  val chain = aPartialFunction.orElse[Int, Int] {
    case 45 => 67
  }
  println(chain(45))

  /*
  HOFs accept partial functions as well
  PF can only have ONE parameter type
   */
}



