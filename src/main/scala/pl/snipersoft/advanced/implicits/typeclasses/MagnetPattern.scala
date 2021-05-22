package pl.snipersoft.advanced.implicits.typeclasses

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object MagnetPattern extends App {

  traditionalOverloadingSolution()
  magnetPatternSolution()
  liftingWorks()
  callByNameNotWorks()

  class P2PRequest
  class P2PResponse
  class Serializer[T]

  def traditionalOverloadingSolution(): Unit = {
    trait Actor {
      def receive(statusCode: Int): Int
      def receive(request: P2PRequest): Int
      def receive(response: P2PResponse): Int
      //    def receive[T](request: T)(implicit serializer: Serializer[T]): Int
      def receive[T : Serializer](message: T): Int
      def receive[T : Serializer](message: T, statusCode: Int): Int
      def receive(future: Future[P2PRequest]): Int
      //    def receive(future: Future[P2PResponse]): Int //not compiling: generic types are cleared
      //...lots of overload
    }
  }

  def magnetPatternSolution(): Unit = {
    trait MessageMagnet[Result] {
      def apply(): Result
    }

    def receive[R](magnet: MessageMagnet[R]): R = magnet()

    implicit class FromP2PRequest(request: P2PRequest) extends MessageMagnet[Int] {
      override def apply(): Int = {
        println("Handling P2P request")
        42
      }
    }

    implicit class FromP2PResponse(response: P2PResponse) extends MessageMagnet[Int] {
      override def apply(): Int = {
        println("Handling P2P response")
        24
      }
    }

    implicit class FromFutureP2PRequest(future: Future[P2PRequest]) extends MessageMagnet[Int] {
      override def apply(): Int = {
        println("Handling future P2P response")
        12
      }
    }

    implicit class FromFutureP2PResponse(future: Future[P2PResponse]) extends MessageMagnet[Int] {
      override def apply(): Int = {
        println("Handling future P2P response")
        18
      }
    }

    receive(new P2PRequest)
    receive(new P2PResponse)

    //no more type erasure problem - code below is not possibly with overloading
    receive(Future(new P2PRequest))
    receive(Future(new P2PResponse))
  }

  def liftingWorks(): Unit = {
    trait MathLib { //magnet pattern solution below
      def add1(x: Int): Int = x+1
      def add1(s: String): Int = s.toInt+1
    }

    trait AddMagnet { //no generic here - witt type lifting not works
      def apply(): Int
    }

    def add1(magnet: AddMagnet): Int = magnet()

    implicit class AddInt(x: Int) extends AddMagnet {
      override def apply(): Int = x+1
    }

    implicit class AddString(s: String) extends AddMagnet {
      override def apply(): Int = s.toInt+1
    }

    val addFunctionValue = add1 _ //not possible in traditional way
    println(addFunctionValue(2))
    println(addFunctionValue("3"))

    //  val receiveFunctionValue = receive _
    //  receiveFunctionValue(new P2PRequest) //not compiling due to generic type
  }

  def callByNameNotWorks(): Unit = {
    class Handler {
      def handle(s: => String): Unit = {
        println(s)
        println(s)
      }
    }

    trait HandlerMagnet {
      def apply(): Unit
    }

    def handle(magnet: HandlerMagnet) = magnet()

    implicit class StringHandlerMagnet(s: => String) extends HandlerMagnet {
      override def apply(): Unit = {
        println(s)
        println(s)
      }
    }

    def sideEffectMethod(): String = {
      println("Log me")
      "Result 1"
    }

    handle(sideEffectMethod())

    handle {
      println("Log me2") //we can miss some logs
      "Result 2" //only this value is converted by implicit magnet class
    }
  }
}
