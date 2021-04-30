package pl.snipersoft.concurrency

import scala.collection.mutable
import scala.util.Random

object ProducerConsumerProblem extends App {

//  naive()
//  waitAndNotify()
  buffer()

  class Container {
    private var value: Int = 0

    def isEmpty: Boolean = value == 0

    def set(newValue: Int): Unit = value = newValue

    def get = {
      val result = value
      value = 0
      result
    }
  }

  def naive(): Unit = {
    val container = new Container

    val consumer = new Thread(() => {
      println("[consumer] waiting...")
      while (container.isEmpty) {
        println("[consumer] actively waiting...")
      }

      println(s"[consumer] I have consumed ${container.get}")
    })

    val producer = new Thread(() => {
      println("[producer] computing")
      Thread.sleep(2000)
      val value = 42
      println(s"[producer] I have produced $value")
      container.set(value)
    })

    consumer.start()
    producer.start()
  }

  def waitAndNotify(): Unit = {
    val container = new Container

    val consumer = new Thread(() => {
      println("[consumer] waiting...")
      container.synchronized {
        container.wait()
      }

      println(s"[consumer] I have consumed ${container.get}")
    })

    val producer = new Thread(() => {
      println("[producer] computing")
      Thread.sleep(2000)
      val value = 42
      container.synchronized {
        println(s"[producer] I have produced $value")
        container.set(value)
        container.notify()
      }
    })

    consumer.start()
    producer.start()
  }

  def buffer(): Unit = {
    val buffer: mutable.Queue[Int] = new mutable.Queue[Int]
    val capacity = 3

    val consumer = new Thread(() => {
      val random = new Random()

      while (true) {
        buffer.synchronized {
          if (buffer.isEmpty) {
            println("[consumer] buffer is empty, waiting...")
            buffer.wait()
          }

          val x = buffer.dequeue()
          println(s"[consumer] I have consumed $x")

          buffer.notify()
        }

        Thread.sleep(random.nextInt(500))
//        Thread.sleep(random.nextInt(250))
//        Thread.sleep(random.nextInt(1000))
      }
    })

    val producer = new Thread(() => {
      val random = new Random()
      var i = 0

      while (true) {
        buffer.synchronized {
          if (buffer.size == capacity) {
            println("[producer] buffer is full, waiting...")
            buffer.wait()
          }

          println(s"[producer] producing value $i")
          buffer.enqueue(i)

          buffer.notify()

          i += 1

          Thread.sleep(500)
        }
      }
    })

    consumer.start()
    producer.start()
  }
}
