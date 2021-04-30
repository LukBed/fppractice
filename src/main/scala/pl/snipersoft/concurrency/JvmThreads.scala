package pl.snipersoft.concurrency

import java.util.concurrent.{ExecutorService, LinkedBlockingQueue, ThreadPoolExecutor, TimeUnit}

object JvmThreads extends App {

  inceptionThreadsExercise()

  def threadPool() = {
    //JVM się wyłącza po zakończeniu działania wszystkich niedemonicznych wątków

    //  val pool = Executors.newFixedThreadPool(10)
    val pool: ExecutorService = new ThreadPoolExecutor(10, 10, 0L, TimeUnit.MILLISECONDS, new LinkedBlockingQueue[Runnable](100))
    pool.execute(() => println("Hello from the thread pool"))
    pool.execute(() => {
      Thread.sleep(1000)
      println("Done after one sec")
    })

    pool.execute(() => {
      Thread.sleep(1000)
      println("Almost done")
      Thread.sleep(1000)
      println("Done after two ses")
    })

    pool.shutdown() //inne wątki się wykonają
    println(pool.isShutdown)
    pool.execute(() => println("Exception in the calling thread!"))
    pool.shutdownNow() //zatrzymuje też inne wątki
  }

  def synchronizedAccess(): Unit = {
    class Account(var amount: Int)
    class SafeAccount(@volatile var amount: Int)

    def buySafe(account: Account, thing: String, price: Int): Unit = account.synchronized {
      account.amount -= price
      println(s"I've bought $thing and I have ${account.amount} now")
    }
  }

  def inceptionThreadsExercise(): Unit = {
    /*
    Construct 50 "inception" threads
    t1 => t2 => ...
    print hellos in reverse order
     */

    def execute(n: Int): Unit = {
      if (n > 0) {
        val thread = new Thread(() => execute(n-1))
        thread.start()
        thread.join() //wait until thread will be finished
      }
      println(n)
    }

    execute(50)

  }

  def quiz(): Unit = {
    var x = 0
    val threads = (1 to 100).map(_ => new Thread(() => x += 1))
    threads.foreach(_.start())

    //max possible value: 100
    //min possible value: 1
  }

  def sleepFallacy(): Unit = {
    var message = ""
    val awesomeThread = new Thread(() => {
      Thread.sleep(1000)
      message = "Scala is awesome"
    })

    message = "Scala sucks"
    awesomeThread.start()
    Thread.sleep(2000) //or 1001
    //awesomeThread.join() //fix problem
    println(message)

    /*
    what's the value of message? is it guarantee?
    almost always Scala is awesome but it's not guarantee
    Thread.sleep not guarantee exactly numbers of mls
     */
  }
}