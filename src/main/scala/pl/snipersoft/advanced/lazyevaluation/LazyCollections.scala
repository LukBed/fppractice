package pl.snipersoft.advanced.lazyevaluation

object LazyCollections extends App {
  def lessThan30(n: Int): Boolean = {
    println(s"Is $n less than 30?")
    n < 30
  }

  def greaterThan20(n: Int): Boolean = {
    println(s"Is $n greater than 20?")
    n > 20
  }

  val myList = List(1,5,23,25,40,45)

  println("Standard")
  println(myList.filter(lessThan30).filter(greaterThan20)) //standard
  println
  
  println("Wrapped lazy - no side effects")
  println(myList.withFilter(lessThan30).withFilter(greaterThan20)) //wrapped lazy
  println
  
  println("Executed lazy")
  myList.withFilter(lessThan30).withFilter(greaterThan20).foreach(println) //executed  lazy
  println
  
  //for-comprehensions use withFilter with guards
  println("For-comprehensions")
  for {
    a <- List(1,2,3) if a%2 == 0 //use lazy vals
  } yield  a+1
  //the same as List(1,2,3).withFilter(_%2 == 0).map(_+1)
}