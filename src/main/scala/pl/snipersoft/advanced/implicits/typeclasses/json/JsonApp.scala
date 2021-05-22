package pl.snipersoft.advanced.implicits.typeclasses.json

import java.util.Date

object JsonApp extends App {

  val janek = User("Janek88", 31, "janek@jankowo.pl")
  val posts = List(
    Post("Pierwszy post", new Date()),
    Post("Drugi post", new Date()),
    Post("trzeci post", new Date()),
  )
  val feed = Feed(janek, posts)

  import pl.snipersoft.advanced.implicits.typeclasses.json.JsonSerialization._

  println(feed.toJson)
}
