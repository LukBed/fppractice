package pl.snipersoft.advanced.darksyntaxsugar

object SpecialMethods extends App {
  class MyStream[T] {
    def -->:(value: T): MyStream[T] = ???
  }

  val myStream = 1 -->: 2 -->: 3 -->: new MyStream[Int]
}