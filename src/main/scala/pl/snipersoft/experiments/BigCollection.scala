package pl.snipersoft.experiments

import java.util.UUID

object BigCollection extends App {
  Thread.sleep(5000)
  val start = freeMemory()

  def uuid() = UUID.randomUUID().toString

  def category() = Category(uuid(), uuid())

  def tank() = Tank(uuid(), uuid(),
    List(category(), category(), category(), category(), category()),
    List(category(), category(), category(), category(), category()),
    List(category(), category(), category(), category(), category()),
    List(uuid(), uuid(), uuid(), uuid(), uuid(), uuid()))

  case class Category(id: String, name: String)

  case class Tank(id: String, name: String, categories1: List[Category], categories2: List[Category], categories3: List[Category], names: List[String])

  val tanks: List[Tank] = (1 to 100_000).map(_ => tank()).toList
  val end = freeMemory()

  println(start)
  println(end)
  val a = System.nanoTime()
  println(System.nanoTime())
  val x = tanks.filter(t => t.id.startsWith("a") || t.name.startsWith("b") || t.categories1.count(_.name.startsWith("c")) > 2 || t.categories2.count(_.name.startsWith("c")) > 2)
  println(x.size + " records")

  val seconds = 1.0 * (System.nanoTime() - a) / 1000000000.0
  println(s"$seconds seconds")

  def freeMemory(): String = {
    val rt = Runtime.getRuntime
    val usedMB = (rt.totalMemory - rt.freeMemory) / 1024 / 1024
    s"Used megabytes: $usedMB"
  }

}
