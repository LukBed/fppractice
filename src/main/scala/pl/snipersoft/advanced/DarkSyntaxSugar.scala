package pl.snipersoft.advanced

import scala.util.Try

object MethodWithSingleParam extends App {
  def myMethod(arg: Int): String = s"Arg: $arg"
  val x = myMethod{42}

  val myTry = Try { throw new RuntimeException }

  List(1,2,3).map{_+1}
}

object SingleAbstractMethodPattern extends App {
  trait Action {
    def act(x: Int): Int
    def doIt(): Unit = println("General Kenobi")
  }

  val myAction: Action = (x: Int) => x + 1

  val myRunnable: Runnable = () => println("Hello")
}

object SpecialMethods extends App {
  class MyStream[T] {
    def -->:(value: T): MyStream[T] = ???
  }

  val myStream = 1 -->: 2 -->: 3 -->: new MyStream[Int]
}

object MultiWordMethodNaming extends App {
  case class TeenGirl(name: String) {
    def `and then said`(gossip: String) = println(s"$name said $gossip")
  }

  val marysia = TeenGirl("Marysia")
  marysia `and then said` "I like Scala"
}

object InfixTypes extends App {
  class Composite[A, B]
  val firstComposite: Composite[Int, String] = ???
  val secondComposite: Int Composite String = ???

  class -->[A, B]
  val towards: Int --> String = ???
}

object UpdateMethod extends App { //used in mutable collections
  val anArray = Array(1,2,3)
  anArray(2) = 7 // anArray.update(2,7)
}

object Setters extends App {
  class Mutable {
    private var internalMember: Int = 0
    def member = internalMember //getter
    def member_=(value: Int): Unit = internalMember = value //setter
  }

  val aMutableContainer = new Mutable
  aMutableContainer.member = 42 // aMutableContainer.member_=(42)
}