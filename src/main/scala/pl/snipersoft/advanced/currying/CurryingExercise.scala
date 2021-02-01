package pl.snipersoft.advanced.currying

object CurryingExercise extends App {

  val simpleAddFunction = (x: Int, y: Int) => x+y
  def simpleAddMethod(x: Int, y: Int) = x+y
  def curriedAddMethod(x: Int)(y: Int) = x+y

  //prepare as many different implementation of add 7 as possible

  val add7_1 = (y: Int) => simpleAddFunction(7, y)
  val add7_2 = (y: Int) => simpleAddMethod(7, y)
  val add7_3 = (y: Int) => curriedAddMethod(7)(y)
  val add7_4 = simpleAddFunction.curried(7)
  val add7_5 = curriedAddMethod(7) _ //PAF
  val add7_6 = curriedAddMethod(7)(_) //PAF - alternative syntax
  val add7_7 = simpleAddMethod(7, _: Int) //alternative syntax for turning methods into function values
  val add7_8 = simpleAddFunction(7, _: Int)
  val add7_9: Int => Int = curriedAddMethod(7)
  val add7_10 = (y: Int) => (simpleAddMethod _)(7, y)
  val add7_11 = (y: Int) => (curriedAddMethod _)(7)(y)

  println(add7_1(-6))
  println(add7_2(-5))
  println(add7_3(-4))
  println(add7_4(-3))
  println(add7_5(-2))
  println(add7_6(-1))
  println(add7_7(0))
  println(add7_8(1))
  println(add7_9(2))
  println(add7_10(3))
  println(add7_11(4))
}
