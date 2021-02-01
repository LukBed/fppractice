package pl.snipersoft.advanced.lazyevaluation

object CallByNameProblem extends App {
  def byNameMethod(n: => Int): Int = n+n+n+1
  def byNameMethod_callByNeed(n: => Int): Int = {
    lazy val t = n
    t+t+t+1
  }

  def retrieveMagicValue = {
    println("waiting")
    Thread.sleep(1000)
    42
  }

  //we'll see waiting 3 times => should be replaced by lazy value
  println(byNameMethod(retrieveMagicValue))
  println(byNameMethod_callByNeed(retrieveMagicValue))
}
