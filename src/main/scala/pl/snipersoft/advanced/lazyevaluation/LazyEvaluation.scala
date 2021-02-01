package pl.snipersoft.advanced.lazyevaluation

object LazyEvaluation extends App {
  lazy val x: Int = {
    println("Hello")
    42
  }

  println(x)
  println(x)
}
