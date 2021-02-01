package pl.snipersoft.advanced.currying

object UnderscoreIntro extends App {
  def hello(a: String, b: String, c: String) = a+b+c
  val insertName = hello("Hi, ", _: String, ", how re you?")
  println(insertName("Marysia"))
}

object Numbers extends App {
  def printNumber(format: String, numbers: List[Double]) = numbers.map(format.format(_)).mkString(" ")
  val firstPrint = printNumber("%4.2f", _: List[Double])
  val secondPrint = printNumber("%8.6f", _: List[Double])
  val thirdPrint = printNumber("%14.12f", _: List[Double])

  val numbers = List(Math.PI, 1.0/3, 1.0/6, 20.0/7)
  println(firstPrint(numbers))
  println(secondPrint(numbers))
  println(thirdPrint(numbers))
}

object NumbersLiftVersion extends App {
  def printNumber(format: String)(numbers: List[Double]) = numbers.map(format.format(_)).mkString(" ")
  val firstPrint = printNumber("%4.2f") _
  val secondPrint = printNumber("%8.6f") _
  val thirdPrint = printNumber("%14.12f") _

  val numbers = List(Math.PI, 1.0/3, 1.0/6, 20.0/7)
  println(firstPrint(numbers))
  println(secondPrint(numbers))
  println(thirdPrint(numbers))
}

object FunctionsVsMethods extends App {
  def byName(n: => Int) = n+1
  def byFunction(f: () => Int) = f() + 1

  def method: Int = 42
  def parentMethod(): Int = 42

  byName(5)
  byName(method)
  byName(parentMethod())
  byName(parentMethod)
//  byName(parentMethod _) //NOK
//  byName(() => 5) //NOK
  byName((() => 5)())

//  byFunction(5) //NOK
//  byFunction(method) //NOK - not eta expansion
  byFunction(() => method)
  byFunction(parentMethod) //eta expansion
  byFunction(parentMethod _)
  byFunction(() => 5)
}