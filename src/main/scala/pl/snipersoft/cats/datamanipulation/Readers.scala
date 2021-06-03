package pl.snipersoft.cats.datamanipulation

import cats.data.Reader

//pure FP dependency injection
object Readers extends App {

  case class Configuration(dbUsername: String, dbPassword: String, host: String, port: Int, nThreads: Int, emailReplyTo: String)

  case class DbConnection(username: String, password: String) {
    def getOrderStatus(orderId: Long): String = "dispatched"

    def getLastOrderId(username: String): Long = 255
  }

  case class HttpService(host: String, port: Int) {
    def start(): Unit = println("Server started")
  }

  case class EmailService(emailReplyTo: String) {
    def sendEmail(address: String, contents: String) = s"From: $emailReplyTo; To: $address >>>> $contents"
  }

  val configuration = Configuration("myUser", "myPass", "localhost", 8080, 4, "admin@page.com")
  val dbReader: Reader[Configuration, DbConnection] = Reader(conf => DbConnection(conf.dbUsername, conf.dbPassword))
  val dbConnection = dbReader.run(configuration)

  val orderStatusReader: Reader[Configuration, String] = dbReader.map(_.getOrderStatus(50))
  val orderStatus = orderStatusReader.run(configuration)

  def getLastOrderStatus(username: String): String = {
    val reader = for {
      orderId <- dbReader.map(_.getLastOrderId(username))
      status <- dbReader.map(_.getOrderStatus(orderId))
    } yield status
    reader.run(configuration)
  }

  println(getLastOrderStatus("testMe"))

  def emailUser(username: String, userEmail: String): String = {
    val reader = for {
      lastOrderId <- dbReader.map(_.getLastOrderId(username))
      orderStatus <- dbReader.map(_.getOrderStatus(lastOrderId))
      emailService <- Reader[Configuration, EmailService](conf => EmailService(conf.emailReplyTo))
    } yield emailService.sendEmail(userEmail, s"Your last order has the status: $orderStatus")

    reader.run(configuration)
  }

  println(emailUser("Janek", "janek@dot.pl"))
}
