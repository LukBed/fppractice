package pl.snipersoft.advanced.implicits.typeclasses.json

import java.util.Date

case class User(name: String, age: Int, email: String)
case class Post(content: String, createdAt: Date)
case class Feed(user: User, posts: Seq[Post])