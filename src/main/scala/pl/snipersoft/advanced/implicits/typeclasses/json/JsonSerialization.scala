package pl.snipersoft.advanced.implicits.typeclasses.json

import java.text.SimpleDateFormat
import java.util.Date

trait JsonSerializer[T] {
  def toJson(v: T): String
}

object JsonSerialization {

  implicit class JsonEnrichment[T](val v: T) extends AnyVal {
    def toJson(implicit serializer: JsonSerializer[T]): String = serializer.toJson(v)
  }

  implicit class EnrichmentSeq[T](val v: Seq[T]) extends AnyVal {
    def toJson(implicit serializer: JsonSerializer[T]): String = s"[${v.map(_.toJson).mkString(", ")}]"
  }

  implicit object StringJsonSerializer extends JsonSerializer[String] {
    override def toJson(v: String): String = "\"" + v + "\""
  }

  implicit object IntJsonSerializer extends JsonSerializer[Int] {
    override def toJson(v: Int): String = v.toString
  }

  implicit object DateJsonSerializer extends JsonSerializer[Date] {
    val dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
    override def toJson(v: Date): String = "\"" + dateFormat.format(v) + "\""
  }

  implicit object MapJsonSerializer extends JsonSerializer[Map[String, String]] {
    override def toJson(v: Map[String, String]): String =
      "{" + v.toSeq.map(p => "\"" + p._1 + "\": " + p._2).mkString(",\n") + "}"
  }

  implicit object UserJsonSerializer extends JsonSerializer[User] {
    override def toJson(v: User): String =
      Map("name" -> v.name.toJson, "age" -> v.age.toJson, "email" -> v.email.toJson).toJson
  }

  implicit object PostJsonSerializer extends JsonSerializer[Post] {
    override def toJson(v: Post): String =
      Map("content" -> v.content.toJson, "createdAt" -> v.createdAt.toJson).toJson
  }

  implicit object FeedJsonSerializer extends JsonSerializer[Feed] {
    override def toJson(v: Feed): String =
      Map("user" -> v.user.toJson, "posts" -> v.posts.toJson).toJson
  }
}