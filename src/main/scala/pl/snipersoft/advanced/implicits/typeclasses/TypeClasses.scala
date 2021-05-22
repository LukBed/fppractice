package pl.snipersoft.advanced.implicits.typeclasses

object TypeClasses extends App {
  case class User(name: String, age: Int, mail: String)

  val janek = User("Janek", 25, "janek@jankowo.pl")

  //type class
  trait HTMLSerializer[T] {
    def serialize(value: T): String
  }

  object HTMLSerializer {
    def serialize[T](value: T)(implicit serializer: HTMLSerializer[T]): String = serializer.serialize(value)

    def apply[T](implicit serializer: HTMLSerializer[T]): HTMLSerializer[T] = serializer
  }

  //type class instance
  implicit object UserHTMLSerializer extends HTMLSerializer[User] {
    override def serialize(u: User): String = s"<div>${u.name}, ${u.age} (<i>${u.mail}</i>)</div>"
  }

  implicit object IntSerializer extends HTMLSerializer[Int] {
    override def serialize(value: Int): String = s"<div><i>$value</i></div>"
  }

  println(HTMLSerializer.serialize(janek))
  println(HTMLSerializer.serialize(45))

  //access to entire type class methods
  println(HTMLSerializer[User].serialize(janek))


  //witch implicit class
  implicit class HTMLEnrichment[T](value: T) {
    def toHTML(implicit serializer: HTMLSerializer[T]): String = serializer.serialize(value)
  }

  println(janek.toHTML)
  println(2.toHTML)

  /*
  -extend to new types
  -choose implementation
  -super expresive
   */


  //context bounds
  def htmlBoilerplate[T](value: T)(implicit serializer: HTMLSerializer[T]): String =
    s"<html><body>${value.toHTML(serializer)}</body></html>"

  //if we need below serializer instances we can use implicitly method
  def htmlSugar[T : HTMLSerializer](value: T): String =
    s"<html><body>${value.toHTML}</body></html>"

  val standardSerializer = implicitly[HTMLSerializer[User]]

  //out implementation
  def myImplicitly[T](implicit v: T): T = v
}
