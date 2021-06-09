package pl.snipersoft.cats.alien

import cats.Monoid

object InvariantFunctors extends App {

  //how we can support ints, doubles, Option[String]?
  trait Crypto[A] { self =>
    def encrypt(value: A): String
    def decrypt(encrypted: String): A

    def imap[B](back: B => A, forth: A => B): Crypto[B] = new Crypto[B] {
      override def encrypt(value: B): String = self.encrypt(back(value))
      override def decrypt(encrypted: String): B = forth(self.decrypt(encrypted))
    }
  }

  def encrypt[A](value: A)(implicit crypto: Crypto[A]): String = crypto.encrypt(value)
  def decrypt[A](encrypted: String)(implicit crypto: Crypto[A]): A = crypto.decrypt(encrypted)

  implicit val caesarCrypto: Crypto[String] = new Crypto[String] {
    override def encrypt(value: String): String = value.map(c => (c+2).toChar)
    override def decrypt(encrypted: String): String = encrypted.map(c => (c-2).toChar)
  }

  implicit val doubleCrypto: Crypto[Double] = caesarCrypto.imap(_.toString, _.toDouble)

//  implicit val optionStringCrypto: Crypto[Option[String]] =
//    caesarCrypto.imap(_.getOrElse(""), s => if (s.isEmpty) None else Some(s))

  implicit def optionCrypto[T](implicit c: Crypto[T], m: Monoid[T]): Crypto[Option[T]] =
    c.imap(_.getOrElse(m.empty), Option(_))

  def test[T](input: T)(implicit crypto: Crypto[T]) {
    val encrypted = encrypt(input)
    val decrypted = decrypt[T](encrypted)
    println(encrypted)
    println(decrypted)
    println("----")
  }

  test("Let's encrypt!")
  test(Math.PI)
  test(Option("Abc"))
  test[Option[String]](None)
  test(Option(5.123532))

  import cats.Invariant
  import cats.Show
  val showOptionString = Invariant[Show].imap(Show[String])(Option(_))(_.getOrElse(""))
  val s = showOptionString.show(Option("qwe"))
  println(s)

  import cats.syntax.invariant._

  val showOptionStringShorter = Show[String].imap(Option(_))(_.getOrElse(""))
  val s2 = showOptionStringShorter.show(Option("asd"))
  println(s2)

  //W - wrapper
  trait MyInvariant[W[_]] { //invariant functor - forth and back
    def imap[A, B](wa: W[A])(forth: A => B)(back: B => A): W[B]
  }

  trait MyContravariant[W[_]] extends MyInvariant[W] { //contravariant functor - only back
    def contramap[A, B](wa: W[A])(back: B => A): W[B]

    override def imap[A, B](wa: W[A])(forth: A => B)(back: B => A): W[B] =
      contramap(wa)(back)
  }

  trait MyFunctor[W[_]] extends MyInvariant[W] { //covariant functor - only forth
    def map[A, B](wa: W[A])(forth: A => B): W[B]

    override def imap[A, B](wa: W[A])(forth: A => B)(back: B => A): W[B] =
      map(wa)(forth)
  }
}


