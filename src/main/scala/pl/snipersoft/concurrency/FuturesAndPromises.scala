package pl.snipersoft.concurrency

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future, Promise}
import scala.util.{Failure, Random, Success}

object FuturesAndPromises extends App {
  promises()

  def futures() {

    def calculate: Int = {
      Thread.sleep(2000)
      42
    }

    import scala.concurrent.ExecutionContext.Implicits.global

    val future = Future {
      calculate //calculate in another thread
    }

    println(future.value) //None - Option[Try[Int]]
    println("Waiting for future")
    future.onComplete {
      case Success(n) => println(s"Calculated $n")
      case Failure(ex) => println(s"Faildes with $ex")
    } //some thread

    Thread.sleep(3000)
  }

  def miniSocialNetwork(): Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global

    case class Profile(id: String, name: String) {
      def poke(anotherProfile: Profile): Unit = {
        println(s"${this.name} poking ${anotherProfile.name}")
      }
    }

    object SocialNetwork {
      val names = Map(
        "fb.id.1-zuck" -> "Mark",
        "fb.id.2-bill" -> "Bill",
        "fb.id.0-dummy" -> "Dummy")

      val friends = Map(
        "fb.id.1-zuck" -> "fb.id.2-bill")

      val random = new Random()

      def fetchProfile(id: String): Future[Profile] = Future {
        Thread.sleep(random.nextInt(300))
        Profile(id, names(id))
      }

      def fetchBestFriend(profile: Profile): Future[Profile] = Future {
        Thread.sleep(400)
        val bestFriendId = friends(profile.id)
        Profile(bestFriendId, names(bestFriendId))
      }
    }

    for {
      mark <- SocialNetwork.fetchProfile("fb.id.1-zuck")
      bill <- SocialNetwork.fetchBestFriend(mark)
    } mark.poke(bill)

    Thread.sleep(1000)

    //fallback
    /*val aProfileNoMatterWhat1st = SocialNetwork.fetchProfile("unknownProfile").recover {
      case e: Throwable => Profile("fb.id.0-dummy", "Forever alone")
    }

    val aProfileNoMatterWhat2nd = SocialNetwork.fetchProfile("unknownProfile").recoverWith {
      case e: Throwable => SocialNetwork.fetchProfile("fb.id.0-dummy")
    }

    //first failed => second failed => return first exception
    val fallbackResult = SocialNetwork.fetchProfile("unknownProfile").fallbackTo {
      case e: Throwable => SocialNetwork.fetchProfile("fb.id.0-dummy")
    }*/
  }

  //blocking is using for critical or transactional cases
  def blocking(): Unit = {
    case class User(name: String)
    case class Transaction(sender: String, receiver: String, amount: Double, status: String)
    import scala.concurrent.ExecutionContext.Implicits.global

    object BankingApp {

      val name = "The Banking App"

      def fetchUser(name: String): Future[User] = Future {
        Thread.sleep(500)
        User(name)
      }

      def createTransaction(user: User, merchantName: String, amount: Double): Future[Transaction] = Future {
        Thread.sleep(1000)
        Transaction(user.name, merchantName, amount, "SUCCESS")
      }

      def purchase(username: String, item: String, merchantName: String, cost: Double): String = {
        //WAIT for transaction to finish
        val transactionStatusFeature = for {
          user <- fetchUser(username)
          transaction <- createTransaction(user, merchantName, cost)
        } yield transaction.status

        Await.result(transactionStatusFeature, 2.seconds)
      }
    }

    println(BankingApp.purchase("Janek", "T-34", "Gustlik", 500))

  }

  def promises(): Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global

    //promise - "controller" over future
    val promise = Promise[Int]()
    val future = promise.future

    //thread 1 - consumer
    future.onComplete {
      case Success(n) => println(s"[consumer] I have received $n")
    }

    //thread 2 - producer
    val producer = new Thread(() => {
      println("[producer] calculating...")
      Thread.sleep(500)
      //fulfilling the promise
      promise.success(42)
      println("[producer] done")
    })

    producer.start()
    Thread.sleep(1000)
  }
}
