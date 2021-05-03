package pl.snipersoft.concurrency

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}
import scala.util.Random

object FuturesAndPromisesExercises extends App {

  //  testFirst()
//  testLast()
  testRetryUntil()

  def inSequence[A, B](first: Future[A], second: Future[B]): Future[B] = first.flatMap(_ => second)

  def first[T](futureA: Future[T], futureB: Future[T]): Future[T] = {
    val promise = Promise[T]()

    /*def completePromise(future: Future[T]): Unit = {
      future.onComplete { result =>
        if (!promise.isCompleted) promise.complete(result)
      }
    }*/

    def completePromise(future: Future[T]): Unit = {
      future.onComplete(promise.tryComplete)
    }

    completePromise(futureA)
    completePromise(futureB)

    promise.future
  }

  def testFirst(): Unit = {
    val a = Future {
      Thread.sleep(500)
      "A"
    }

    val b = Future {
      Thread.sleep(1000)
      "B"
    }

    first(a, b).foreach(println) //A
    first(b, a).foreach(println) //A
    Thread.sleep(2000)
  }

  def last[T](futureA: Future[T], futureB: Future[T]): Future[T] = {
    val firstPromise = Promise[T]()
    val secondPromise = Promise[T]()

    def fulfillPromises(future: Future[T]): Unit = {
      future.onComplete(result => if (!firstPromise.tryComplete(result)) secondPromise.tryComplete(result))
    }

    fulfillPromises(futureA)
    fulfillPromises(futureB)

    secondPromise.future
  }

  def testLast(): Unit = {
    val a = Future {
      Thread.sleep(500)
      "A"
    }

    val b = Future {
      Thread.sleep(1000)
      "B"
    }

    last(a, b).foreach(println) //B
    last(b, a).foreach(println) //B
    Thread.sleep(2000)
  }

  def retryUntil[T](action: () => Future[T], condition: T => Boolean): Future[T] =
    action().flatMap(value =>
      if (condition(value)) retryUntil(action, condition)
      else Future(value))


  def retryUntilByDaniel[T](action: () => Future[T], condition: T => Boolean): Future[T] =
    action()
      .filter(!condition(_))
      .recoverWith { //recover if NoSuchElementException
        case _ => retryUntilByDaniel(action, condition)
      }

  def testRetryUntil(): Unit = {
    val random = new Random()

    val action = () => Future {
      Thread.sleep(100)
      val nextValue = random.nextInt(100)
      println(s"Generated $nextValue")
      nextValue
    }

    retryUntil(action, (x: Int) => x>5).foreach(result => println(s"Result: $result"))
//    retryUntilByDaniel(action, (x: Int) => x>5).foreach(result => println(s"Result: $result"))

    Thread.sleep(10000)
  }
}
